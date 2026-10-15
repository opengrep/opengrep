class Docs
  def in_block
    @b = source()
    respond_to do |format|
      read_b
    end
  end

  def in_nested_block
    @c = source()
    respond_to do |format|
      format.any(:md) do
        read_c
      end
    end
  end

  def in_lambda
    @d = source()
    create_commit(success_path: -> { read_d })
  end

  def constant_in_block
    @e = "constant"
    respond_to do |format|
      read_e
    end
  end

  def read_b
    # ruleid: ruby_ivar_call_in_block
    sink(@b)
  end

  def read_c
    # ruleid: ruby_ivar_call_in_block
    sink(@c)
  end

  def read_d
    # ruleid: ruby_ivar_call_in_block
    sink(@d)
  end

  def read_e
    # ok: ruby_ivar_call_in_block
    sink(@e)
  end
end
