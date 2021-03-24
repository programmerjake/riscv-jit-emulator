// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use super::Pos;
use std::{cell::Cell, mem};

#[derive(Clone, Copy)]
#[repr(C)]
pub struct InputContext<'a> {
    pub file_name: &'a str,
    pub input: &'a str,
}

thread_local! {
    static INPUT_CONTEXT: Cell<Option<InputContext<'static>>> = Cell::new(None);
}

impl Pos {
    pub fn get_input_context<R, F: for<'input> FnOnce(Option<InputContext<'input>>) -> R>(
        f: F,
    ) -> R {
        let input: Option<InputContext> = INPUT_CONTEXT.with(|c| c.get());
        f(input)
    }
    pub fn call_with_input_context<'input, R, F: FnOnce() -> R>(
        input: InputContext<'input>,
        f: F,
    ) -> R {
        struct RestoreOnDrop {
            old_context: Option<InputContext<'static>>,
        }

        struct PanicOnDrop {}

        impl Drop for PanicOnDrop {
            fn drop(&mut self) {
                panic!(
                    "restoring old context failed -- aborting via double-panic to prevent memory unsafety"
                );
            }
        }

        impl Drop for RestoreOnDrop {
            fn drop(&mut self) {
                let v = PanicOnDrop {};
                INPUT_CONTEXT.with(|context| context.set(self.old_context));
                mem::forget(v);
            }
        }

        let restore_on_drop = unsafe {
            INPUT_CONTEXT.with(|context| RestoreOnDrop {
                old_context: context.replace(Some(mem::transmute::<
                    InputContext<'input>,
                    InputContext<'static>,
                >(input))),
            })
        };
        let retval = f();
        drop(restore_on_drop);
        retval
    }
}
