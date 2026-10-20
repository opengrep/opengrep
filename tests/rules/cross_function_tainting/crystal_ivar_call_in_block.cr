class Docs
  def in_block
    @b = source()
    respond_to do |format|
      read_b
    end
  end

  def constant_in_block
    @e = "constant"
    respond_to do |format|
      read_e
    end
  end

  def read_b
    # ruleid: crystal_ivar_call_in_block
    sink(@b)
  end

  def read_e
    # ok: crystal_ivar_call_in_block
    sink(@e)
  end
end
