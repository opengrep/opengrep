template<typename T>
void customForEach(const std::vector<T>& arr, std::function<void(T)> callback) {
    for (const auto& item : arr) {
        callback(item);
    }
}
