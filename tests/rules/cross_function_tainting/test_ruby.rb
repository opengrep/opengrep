class PaginatedUtmLinksPresenter
  def initialize(seller)
    @key = taint
  end

  def props
    # ruleid: test
    query = "SELECT * FROM table WHERE id = #{@key}"
    return query
  end
end

# Test - seller should be tainted and flow to @key
