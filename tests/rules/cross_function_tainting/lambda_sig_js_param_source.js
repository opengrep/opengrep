// A lambda's own (destructured) parameter is a source wherever the lambda is
// analysed.
function createBranch() {
  return axios
    .post(createEndpoint(this.projectPath))
    .then(({ data }) => {
      // ruleid: lambda-sig-js-param-source
      window.location.href = data.url;
    });
}
function fixed() {
  return axios.post(endpoint).then(({ data }) => {
    log(data.url);
    // ok: lambda-sig-js-param-source
    window.location.href = "/home";
  });
}
