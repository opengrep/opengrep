export const ErrorDetails = (
  { error }: { error: TestError }
) => (
  // rule-id: test
  {
    __html: formatDiffMessage(error),
  }
);
