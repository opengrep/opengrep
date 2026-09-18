#include <vector>
#include <string>
#include <functional>
#include <algorithm>

std::string source();
void sink(std::string s);

template <typename T>
void customForEach(const std::vector<T>& arr, std::function<void(T)> callback);

template <typename T>
std::vector<T> customMap(const std::vector<T>& arr, std::function<T(T)> callback);

template <typename T>
std::vector<T> customMapBuiltin(const std::vector<T>& arr, std::function<T(T)> callback);

template <typename T>
void directCall(std::function<void(T)> callback, T value);

std::string getHistory(std::string name, std::string owner);
void toplevelHandler(std::string x);
void test_custom_map();
void test_custom_map_builtin();
void test_custom_foreach();
void test_direct_call();
void test_builtin_for_each();
void test_builtin_transform();
void test_original_example();
