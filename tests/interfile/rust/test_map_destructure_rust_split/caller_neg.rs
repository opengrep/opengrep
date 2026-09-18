use crate::handler_neg::handler_neg;
use crate::Req::Req;

fn caller_neg() {
    handler_neg(Req { body: "safe".to_string(), user: source() });
}
