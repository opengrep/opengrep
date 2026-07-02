template<typename T>
std::vector<T> customMapBuiltin(const std::vector<T>& arr, std::function<T(T)> callback) {
    std::vector<T> result;
    std::transform(arr.begin(), arr.end(), std::back_inserter(result), callback);
    return result;
}
