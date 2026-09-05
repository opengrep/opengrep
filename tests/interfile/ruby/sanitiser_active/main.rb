require_relative 'senders'

def main
  send_clean(source())
  send_dirty(source())
end
