mod senders;

use senders::{send_clean, send_dirty};

fn main() {
    send_clean(source());
    send_dirty(source());
}
