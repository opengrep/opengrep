class Cmp
  def heredoc_form(t)
    # ruleid: ruby_heredoc_sqli
    Database.fetch(<<-SQL)
      SELECT * FROM #{t} WHERE archived = false
    SQL
  end

  def string_form(t)
    # ruleid: ruby_heredoc_sqli
    Database.fetch("SELECT * FROM #{t} WHERE archived = false")
  end

  def not_sql(t)
    # ok: ruby_heredoc_sqli
    Database.fetch(<<-TXT)
      hello #{t}
    TXT
  end
end
