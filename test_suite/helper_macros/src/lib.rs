#[macro_export]
macro_rules! not_bytes_asset {
    ($($t:ty),*) => {
        $(
        impl glace::BytesAsset for $t {
            fn try_bytes(&self) -> glace::Result<std::borrow::Cow<'static, [u8]>> {
                todo!()
            }

            fn try_bytes_modified(
                &self,
                previous_modified: Option<std::time::SystemTime>,
            ) -> glace::Result<Option<(Vec<u8>, std::time::SystemTime)>> {
                todo!()
            }

            fn try_load_bytes(&self) -> glace::Result<Vec<u8>> {
                todo!()
            }
        }
        )*
    }
}
