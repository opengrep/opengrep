
#include <glib.h>

char *get_ip(void *message)
{
  g_autofree char *ip = NULL;

  ip = g_strdup("127.0.0.1");
  return g_steal_pointer (&ip);
}
