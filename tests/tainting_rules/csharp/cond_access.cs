class C
{
    void foo1(){
        string arr[100];
        val x = source();
        arr?[5] = x;
        // ruleid: taint
        sink(arr);
        val y = arr?[5];
        // ruleid: taint
        sink(y);
    }
}
