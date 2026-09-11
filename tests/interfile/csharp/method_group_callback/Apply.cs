class Apply {
    public static void Run(System.Action<string> cb, string data) {
        cb.Invoke(data);
    }
}
