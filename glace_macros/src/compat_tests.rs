use std::mem::size_of;

#[repr(u32)]
pub enum SizeTest {
    _Just(u32),
}

#[repr(u32)]
pub enum LayoutTest {
    Zero,
    One,
    Other(u32),
}

const SIZE_TEST_ERROR: &str = "Your target's `enum` layout is not compatible with this crate. For this proc macro to work correctly, an enum with a u32 discriminant and a u32 field must be the same size as a u64. Your target may be adding padding, or not respecting the #[repr(u32)] attribute.";

const LAYOUT_TEST_ERROR: &str = "Your target's `enum` layout is not compatible with this crate. For this proc macro to work correctly, an enum with a u32 discriminant and a u32 field must be represented with the discriminant filling the lower 32 bits. Additionally, the discriminant must monotonically increase from 0.";

pub fn size_test<T>() {
    assert_eq!(size_of::<T>(), size_of::<u64>(), "{}", SIZE_TEST_ERROR);
}

pub fn discriminant_test(t_as_u64: u64, expected: u64) {
    assert_eq!(
        0x00000000ffffffff & t_as_u64,
        expected,
        "incompatible field representation: {}",
        LAYOUT_TEST_ERROR,
    );
}

pub fn field_test(t_as_u64: u64, expected: u64) {
    assert_eq!(
        t_as_u64 >> 32,
        expected,
        "incompatible discriminant representation: {}",
        LAYOUT_TEST_ERROR,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::transmute;

    #[test]
    fn size_test_pass() {
        size_test::<u64>();
        size_test::<[u32; 2]>();
        size_test::<(u32, u16, u8, u8)>();
        size_test::<SizeTest>();
    }

    #[test]
    #[should_panic]
    fn size_test_fail() {
        size_test::<u32>();
    }

    #[test]
    fn discriminant_test_pass() {
        discriminant_test(unsafe { transmute((111_u32, 222_u32)) }, 111_u64);
        discriminant_test(unsafe { transmute([222_u32, 333_u32]) }, 222_u64);
        discriminant_test(unsafe { transmute(LayoutTest::Zero) }, 0_u64);
        discriminant_test(unsafe { transmute(LayoutTest::One) }, 1_u64);
        discriminant_test(unsafe { transmute(LayoutTest::Other(123)) }, 2_u64);
    }

    #[test]
    #[should_panic]
    fn discriminant_test_fail() {
        discriminant_test(unsafe { transmute((111_u32, 222_u32)) }, 222_u64);
    }

    #[test]
    fn field_test_pass() {
        field_test(unsafe { transmute((123_u32, 456_u32)) }, 456_u64);
        field_test(unsafe { transmute([456_u32, 123_u32]) }, 123_u64);
        field_test(unsafe { transmute(LayoutTest::Other(123)) }, 123_u64);
    }

    #[test]
    #[should_panic]
    fn field_test_fail() {
        field_test(unsafe { transmute((111_u32, 222_u32)) }, 111_u64);
    }
}
