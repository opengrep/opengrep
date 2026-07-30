def run
  x = taint()
  helper.tap do |item|
    # The finding must anchor HERE (the focused block-body sink), not on
    # the taint() origin two lines up.
    # ruleid: block-body-sink-range
    consume(x)
  end
end
