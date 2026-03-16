defmodule Test do
  def run(table, id) do
    # Matches: $EXPR = table ("..." skips the "SELECT * FROM " prefix)
    #ERROR:
    query("SELECT * FROM #{table}")

    # Matches: $EXPR = table ("..." skips the prefix; trailing "..." skips rest)
    #ERROR:
    query("SELECT * FROM #{table} WHERE id = #{id}")

    # No match: no interpolation
    query("SELECT * FROM users")
  end
end
