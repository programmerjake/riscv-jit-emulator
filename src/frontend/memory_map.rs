// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use super::{ParseError, ParseResult};
use alloc::{boxed::Box, vec::Vec};
use core::{
    convert::TryInto,
    fmt,
    ops::{Deref, Range},
};

#[derive(Debug)]
pub(crate) struct AddressUnmapped {
    pub(crate) address: u64,
}

impl From<AddressUnmapped> for ParseError {
    fn from(v: AddressUnmapped) -> Self {
        make_error!("accessed unmapped address: 0x{:X}", v.address)
    }
}

struct DebugPageRow<'a> {
    address: u64,
    bytes: &'a [u8],
}

impl fmt::Debug for DebugPageRow<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x{:04X}:", self.address)?;
        for (index, &byte) in self.bytes.iter().enumerate() {
            if index != 0 && index % 8 == 0 {
                write!(f, " ")?;
            }
            write!(f, " {:02X}", byte)?;
        }
        Ok(())
    }
}

struct DebugPageData<'a> {
    address: u64,
    bytes: &'a [u8; Page::SIZE],
}

impl DebugPageData<'_> {
    fn debug_fmt(&self, debug_list: &mut fmt::DebugList<'_, '_>) {
        const ROW_SIZE: usize = 0x20;
        for offset in (0..Page::SIZE).step_by(ROW_SIZE) {
            debug_list.entry(&DebugPageRow {
                address: offset as u64 + self.address,
                bytes: &self.bytes[offset..][..ROW_SIZE],
            });
        }
    }
}

impl fmt::Debug for DebugPageData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_list = f.debug_list();
        self.debug_fmt(&mut debug_list);
        debug_list.finish()
    }
}

#[derive(Clone)]
pub(crate) enum PageData<'a> {
    Borrowed(&'a [u8; Page::SIZE]),
    Owned(Box<[u8; Page::SIZE]>),
}

impl fmt::Debug for PageData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::Borrowed(bytes) => f
                .debug_tuple("Borrowed")
                .field(&DebugPageData { address: 0, bytes })
                .finish(),
            Self::Owned(bytes) => f
                .debug_tuple("Owned")
                .field(&DebugPageData { address: 0, bytes })
                .finish(),
        }
    }
}

impl<'a> PageData<'a> {
    pub(crate) fn into_owned(self) -> Box<[u8; Page::SIZE]> {
        match self {
            Self::Borrowed(v) => Box::<[u8]>::from(&v[..]).try_into().unwrap(),
            Self::Owned(v) => v,
        }
    }
    pub(crate) fn to_mut(&mut self) -> &mut Box<[u8; Page::SIZE]> {
        match *self {
            Self::Borrowed(v) => {
                *self = Self::Owned(Box::<[u8]>::from(&v[..]).try_into().unwrap());
            }
            _ => {}
        }
        match self {
            Self::Borrowed(_) => unreachable!(),
            Self::Owned(v) => v,
        }
    }
    pub(crate) fn borrowed_ref(&self) -> Option<&'a [u8; Page::SIZE]> {
        match *self {
            Self::Borrowed(v) => Some(v),
            Self::Owned(_) => None,
        }
    }
}

impl Deref for PageData<'_> {
    type Target = [u8; Page::SIZE];

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Borrowed(v) => v,
            Self::Owned(v) => v,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Page<'a> {
    pub(crate) data: PageData<'a>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) struct PageIndexAndOffset<I, O> {
    pub(crate) page_index: I,
    pub(crate) offset: O,
}

impl<I, O> PageIndexAndOffset<I, O> {
    pub(crate) fn map_page_index<R, F: FnOnce(I) -> R>(self, f: F) -> PageIndexAndOffset<R, O> {
        let PageIndexAndOffset { page_index, offset } = self;
        PageIndexAndOffset {
            page_index: f(page_index),
            offset,
        }
    }
}

impl<'a> Page<'a> {
    pub(crate) const LOG2_SIZE: usize = 12; // 4KiB
    pub(crate) const SIZE: usize = 1 << Self::LOG2_SIZE;
    pub(crate) const SIZE_MINUS_ONE: usize = Self::SIZE - 1;
    pub(crate) const LOG2_ADDRESS_LIMIT: usize = 38; // 256GiB
    pub(crate) const LOG2_ADDRESS_LIMIT_IN_PAGES: usize =
        Self::LOG2_ADDRESS_LIMIT - Page::LOG2_SIZE;
    pub(crate) const ADDRESS_LIMIT_IN_PAGES: usize = 1 << Self::LOG2_ADDRESS_LIMIT_IN_PAGES;
    pub(crate) const ADDRESS_LIMIT: u64 = 1 << Self::LOG2_ADDRESS_LIMIT;
    pub(crate) const ZERO_PAGE_BYTES: &'static [u8; Page::SIZE] = &[0; Page::SIZE];
    pub(crate) const ZERO_PAGE_DATA: PageData<'static> = PageData::Borrowed(Self::ZERO_PAGE_BYTES);
    pub(crate) const fn unlimited_page_index_and_offset(
        address: u64,
    ) -> PageIndexAndOffset<u64, usize> {
        PageIndexAndOffset {
            page_index: address >> Self::LOG2_SIZE,
            offset: address as usize % Self::SIZE,
        }
    }
    pub(crate) fn page_index_and_offset(
        address: u64,
    ) -> Result<PageIndexAndOffset<usize, usize>, AddressUnmapped> {
        let retval = Self::unlimited_page_index_and_offset(address);
        if retval.page_index < Self::ADDRESS_LIMIT_IN_PAGES as u64 {
            Ok(retval.map_page_index(|v| v as usize))
        } else {
            Err(AddressUnmapped { address })
        }
    }
    pub(crate) fn page_index_range_and_offset(
        address: Range<u64>,
    ) -> Result<Range<PageIndexAndOffset<usize, usize>>, AddressUnmapped> {
        assert!(address.start <= address.end);
        let start = Self::unlimited_page_index_and_offset(address.start);
        let end = Self::unlimited_page_index_and_offset(address.end);
        if end.page_index <= Self::ADDRESS_LIMIT_IN_PAGES as u64 {
            Ok(start.map_page_index(|v| v as usize)..end.map_page_index(|v| v as usize))
        } else {
            Err(AddressUnmapped {
                address: address.start.max(address.end - 1),
            })
        }
    }
}

struct DebugMemoryMapPages<'a> {
    pages: &'a [Option<Page<'a>>],
}

impl fmt::Debug for DebugMemoryMapPages<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_list = f.debug_list();
        for (page_index, bytes) in self
            .pages
            .iter()
            .enumerate()
            .flat_map(|(index, page)| Some((index, &*page.as_ref()?.data)))
        {
            DebugPageData {
                address: page_index as u64 * Page::SIZE as u64,
                bytes,
            }
            .debug_fmt(&mut debug_list);
        }
        debug_list.finish()
    }
}

#[derive(Clone)]
pub(crate) struct MemoryMap<'a> {
    pages: Vec<Option<Page<'a>>>,
}

impl fmt::Debug for MemoryMap<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MemoryMap")
            .field("pages", &DebugMemoryMapPages { pages: &self.pages })
            .finish()
    }
}

impl<'a> MemoryMap<'a> {
    pub(crate) fn new() -> Self {
        Self { pages: Vec::new() }
    }
    pub(crate) fn copy_on_write<'b>(base: &'a MemoryMap<'b>) -> Self {
        Self {
            pages: base
                .pages
                .iter()
                .map(|page| match page {
                    None => None,
                    Some(Page { data }) => Some(Page {
                        data: PageData::Borrowed(data),
                    }),
                })
                .collect(),
        }
    }
    #[cold]
    fn expand_pages_to_include_slow(&mut self, page_index: usize) {
        assert!(page_index < Page::ADDRESS_LIMIT_IN_PAGES);
        while page_index >= self.pages.len() {
            self.pages.push(None);
        }
    }
    fn expand_pages_to_include(&mut self, page_index: usize) {
        if page_index >= self.pages.len() {
            self.expand_pages_to_include_slow(page_index);
        }
    }
    /// sets the range of addresses in `address_range` to the bytes in `bytes`.
    /// if `bytes` is too small, the remaining bytes in `address_range` are set
    /// to zeros. if `bytes` is too big, the remaining bytes in `bytes` are ignored.
    pub(crate) fn map_range_to_bytes(
        &mut self,
        address_range: Range<u64>,
        mut bytes: &'a [u8],
    ) -> ParseResult<()> {
        assert!(address_range.start <= address_range.end);
        if address_range.end > Page::ADDRESS_LIMIT {
            return_error!(
                "tried to map address range that is past limit: address_range=0x{:X}..0x{:X} limit=0x{:X}",
                address_range.start,
                address_range.end,
                Page::ADDRESS_LIMIT
            );
        }
        if address_range.is_empty() {
            return Ok(());
        }
        let mut page_range = Page::page_index_range_and_offset(address_range.clone())?;
        let address_range_len = address_range.end as usize - address_range.start as usize;
        fn map_page<'a>(
            page: &mut Option<Page<'a>>,
            offset_range: Range<usize>,
            bytes: &'a [u8],
        ) -> ParseResult<()> {
            let bytes = &bytes[..bytes.len().min(offset_range.len())];
            if offset_range == (0..Page::SIZE) {
                match bytes.len() {
                    Page::SIZE => {
                        *page = Some(Page {
                            data: PageData::Borrowed(bytes.try_into().unwrap()),
                        });
                        return Ok(());
                    }
                    0 => {
                        *page = Some(Page {
                            data: Page::ZERO_PAGE_DATA,
                        });
                        return Ok(());
                    }
                    _ => {}
                }
            }
            match page {
                None => {
                    let mut page_bytes = Vec::with_capacity(Page::SIZE);
                    page_bytes.resize(offset_range.start, 0);
                    page_bytes.extend(bytes);
                    page_bytes.resize(Page::SIZE, 0);
                    *page = Some(Page {
                        data: PageData::Owned(page_bytes.into_boxed_slice().try_into().unwrap()),
                    });
                }
                Some(Page {
                    data: data @ PageData::Borrowed(_),
                }) => {
                    let mut page_bytes = Vec::with_capacity(Page::SIZE);
                    let old_bytes = data.borrowed_ref().unwrap();
                    page_bytes.extend(&old_bytes[..offset_range.start]);
                    page_bytes.extend(bytes);
                    page_bytes.resize(offset_range.end, 0);
                    page_bytes.extend(&old_bytes[offset_range.end..]);
                    *data = PageData::Owned(page_bytes.into_boxed_slice().try_into().unwrap());
                }
                Some(Page {
                    data: PageData::Owned(data),
                }) => {
                    data[offset_range.start..][..bytes.len()].copy_from_slice(bytes);
                    data[offset_range][bytes.len()..].fill(0);
                }
            }
            Ok(())
        }
        let page_index_range_end = if page_range.end.offset != 0 {
            page_range.end.page_index + 1
        } else {
            page_range.end.page_index
        };
        let page_index_range = page_range.start.page_index..page_index_range_end;
        self.expand_pages_to_include(page_index_range.clone().last().unwrap());
        for page_index in page_index_range {
            let offset_range_start = if page_index == page_range.start.page_index {
                page_range.start.offset
            } else {
                0
            };
            let offset_range_end = if page_index == page_range.end.page_index {
                page_range.end.offset
            } else {
                Page::SIZE
            };
            map_page(
                &mut self.pages[page_index],
                offset_range_start..offset_range_end,
                bytes,
            )?;
            bytes = bytes.get(Page::SIZE - offset_range_start..).unwrap_or(&[]);
        }
        Ok(())
    }
    /// reads bytes into `data` until we hit an unmapped page
    #[must_use]
    pub(crate) fn read_till_unmapped(&self, mut address: u64, mut data: &mut [u8]) -> usize {
        if data.is_empty() {
            return 0;
        }
        let PageIndexAndOffset {
            page_index,
            mut offset,
        } = match Page::page_index_and_offset(address) {
            Ok(v) => v,
            Err(_) => return 0,
        };
        let mut retval = 0;
        for page_index in page_index.. {
            if let Some(page) = self
                .pages
                .get(page_index)
                .and_then(Option::as_ref)
                .map(|v| &*v.data)
            {
                let rest_of_page = &page[offset..];
                if data.len() <= rest_of_page.len() {
                    data.copy_from_slice(&rest_of_page[..data.len()]);
                    retval += data.len();
                    break;
                }
                let (data_current, data_rest) = data.split_at_mut(rest_of_page.len());
                data_current.copy_from_slice(rest_of_page);
                retval += data_current.len();
                data = data_rest;
                address += rest_of_page.len() as u64;
                offset = 0;
            } else {
                break;
            }
        }
        retval
    }
    pub(crate) fn read_fully(&self, address: u64, data: &mut [u8]) -> Result<(), AddressUnmapped> {
        let read_length = self.read_till_unmapped(address, data);
        if read_length != data.len() {
            Err(AddressUnmapped {
                address: address + (data.len() - read_length) as u64,
            })
        } else {
            Ok(())
        }
    }
    pub(crate) fn write_fully(
        &mut self,
        mut address: u64,
        mut data: &[u8],
    ) -> Result<(), AddressUnmapped> {
        if data.is_empty() {
            return Ok(());
        }
        let PageIndexAndOffset {
            page_index,
            mut offset,
        } = Page::page_index_and_offset(address)?;
        for page_index in page_index.. {
            let page = self
                .pages
                .get_mut(page_index)
                .and_then(Option::as_mut)
                .map(|v| v.data.to_mut())
                .ok_or(AddressUnmapped { address })?;
            let rest_of_page = &mut page[offset..];
            if data.len() <= rest_of_page.len() {
                rest_of_page[..data.len()].copy_from_slice(data);
                break;
            }
            let (data_current, data_rest) = data.split_at(rest_of_page.len());
            rest_of_page.copy_from_slice(data_current);
            data = data_rest;
            address += rest_of_page.len() as u64;
            offset = 0;
        }
        Ok(())
    }
    #[cfg(test)]
    /// reference implementation of `read_fully` for a single byte
    fn read_byte_reference(&self, address: u64) -> Result<u8, AddressUnmapped> {
        let PageIndexAndOffset { page_index, offset } = Page::page_index_and_offset(address)?;
        Ok(self
            .pages
            .get(page_index)
            .and_then(Option::as_ref)
            .map(|v| &*v.data)
            .ok_or(AddressUnmapped { address })?[offset])
    }
    #[cfg(test)]
    /// reference implementation of `map_range_to_bytes`
    fn map_range_to_bytes_reference(
        &mut self,
        address_range: Range<u64>,
        bytes: &'a [u8],
    ) -> ParseResult<()> {
        assert!(address_range.start <= address_range.end);
        if address_range.end > Page::ADDRESS_LIMIT {
            return_error!(
                "tried to map address range that is past limit: address_range=0x{:X}..0x{:X} limit=0x{:X}",
                address_range.start,
                address_range.end,
                Page::ADDRESS_LIMIT
            );
        }
        for (index, address) in address_range.enumerate() {
            let PageIndexAndOffset { page_index, offset } = Page::page_index_and_offset(address)?;
            self.expand_pages_to_include(page_index);
            let mut page = self.pages[page_index].take().unwrap_or(Page {
                data: Page::ZERO_PAGE_DATA,
            });
            page.data.to_mut()[offset] = bytes.get(index).copied().unwrap_or(0);
            self.pages[page_index] = Some(page);
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::{MemoryMap, Page};
    use alloc::vec::Vec;
    use std::dbg;

    #[test]
    fn test_map() {
        const STEP: usize = Page::SIZE / 4;
        const LEN: usize = 3 * Page::SIZE;
        let old_page_data: Vec<u8> = (0..LEN)
            .map(|i| 0x10 | ((i * 0x12345) >> 16) as u8 & 0xF)
            .collect();
        let bytes: Vec<u8> = (0..LEN)
            .map(|i| 0x20 | ((i * 0x12327) >> 16) as u8 & 0xF)
            .collect();
        for &page0_index in &[None, Some(0)] {
            for &page1_index in &[None, Some(1)] {
                for &page2_index in &[None, Some(2)] {
                    for map_start_address in (0..LEN).step_by(STEP) {
                        for map_end_address in (map_start_address..LEN).step_by(STEP) {
                            for bytes_len in (0..LEN).step_by(STEP) {
                                let mut memory_map = MemoryMap::new();
                                for page_index in page0_index
                                    .into_iter()
                                    .chain(page1_index)
                                    .chain(page2_index)
                                {
                                    dbg!(page_index);
                                    let range_start = page_index * Page::SIZE;
                                    let range_end = range_start + Page::SIZE;
                                    memory_map
                                        .map_range_to_bytes_reference(
                                            range_start as u64..range_end as u64,
                                            &old_page_data[range_start..range_end],
                                        )
                                        .unwrap();
                                }
                                let mut reference_memory_map = memory_map.clone();
                                let address_range =
                                    map_start_address as u64..map_end_address as u64;
                                dbg!(&address_range);
                                dbg!(bytes_len);
                                memory_map
                                    .map_range_to_bytes(address_range.clone(), &bytes[..bytes_len])
                                    .unwrap();
                                reference_memory_map
                                    .map_range_to_bytes_reference(
                                        address_range,
                                        &bytes[..bytes_len],
                                    )
                                    .unwrap();
                                for page_index in
                                    0..memory_map.pages.len().max(reference_memory_map.pages.len())
                                {
                                    dbg!(page_index);
                                    let page = memory_map
                                        .pages
                                        .get(page_index)
                                        .and_then(|v| v.as_ref())
                                        .map(|v| &*v.data);
                                    let reference_page = reference_memory_map
                                        .pages
                                        .get(page_index)
                                        .and_then(|v| v.as_ref())
                                        .map(|v| &*v.data);
                                    assert_eq!(page, reference_page);
                                }
                                if bytes_len > map_end_address - map_start_address {
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
