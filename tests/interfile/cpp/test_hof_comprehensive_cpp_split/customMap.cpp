template<typename T>
std::vector<T> customMap(const std::vector<T>& arr, std::function<T(T)> callback) {
    std::vector<T> result;
    for (const auto& item : arr) {
        result.push_back(callback(item));
    }
    return result;
}
