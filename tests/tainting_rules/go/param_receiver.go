// ParamReceiver was previously translated to ParamFixme, so the receiver
// variable was invisible to the taint engine (Fold_IL_params.fold skipped it).
// It is now translated to Param so focus-metavariable picks it up.

func (r *SomeType) Dangerous() {
	x := r.Field
	// ruleid: taint
	sink(x)
}

// Regular function — not a receiver method, so $RECV never binds here.
func NotAMethod(r *SomeType) {
	x := r.Field
	// ok: taint
	sink(x)
}
