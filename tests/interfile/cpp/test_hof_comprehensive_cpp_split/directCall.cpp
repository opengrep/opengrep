template<typename T>
void directCall(std::function<void(T)> callback, T value) {
    callback(value);
}
