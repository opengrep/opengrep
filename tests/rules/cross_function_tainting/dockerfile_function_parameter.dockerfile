FROM debian

# ruleid: dockerfile_function_parameter
RUN relay() { eval "$1"; }; relay "$(get_input)"
# ok: dockerfile_function_parameter
RUN quiet() { eval "$2"; }; quiet "$(get_input)" safe
