def go
  account = Account.new
  account.credit(source())
  account.debit(source())
end
