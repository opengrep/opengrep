class Item
{
    static int count;

    public Item() { count = Item.Total(); }

    static int Total() { return count; }
}
