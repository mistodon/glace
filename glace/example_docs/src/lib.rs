pub use glace as tempname;
use glace::glace;

glace! {
    #[path = "example_assets"]
    pub mod example_assets {
        use crate::tempname as glace;
    }
}
