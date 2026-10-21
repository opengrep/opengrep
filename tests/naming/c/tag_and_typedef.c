struct node {
  int value;
  struct node *next;
};

typedef struct node node;

int second(node *head) {
  struct node *next = head->next;
  return next->value;
}
