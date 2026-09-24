# cf. https://github.com/PyCQA/bandit/blob/d5f8fa0d89d7b11442fc6ec80ca42953974354c8/examples/paramiko_injection.py

import paramiko
from paramiko import client


client = paramiko.client.SSHClient()
client.connect("somehost")

# ok:paramiko-exec-command
client.exec_command("ls -r /")

# ruleid:paramiko-exec-command
client.exec_command(user_input)

client2 = client.SSHClient()
client2.connect("somehost")

# ok:paramiko-exec-command
client2.exec_command("ls -r /")

# client is the connection assigned above, so client2 is not a paramiko client
# ok:paramiko-exec-command
client2.exec_command(user_input)
