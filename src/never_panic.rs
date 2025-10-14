#[allow(unused_macros)]
macro_rules! never_panic {
    () => {
        never_panic!()
    };
}

#[allow(unused_imports)]
use never_panic as panic;
