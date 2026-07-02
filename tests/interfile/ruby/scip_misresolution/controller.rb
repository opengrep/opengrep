class Controller
  def create
    @report = Report.new(report_params)
    # ruleid: test-scip-misresolution
    sink(@report)
  end

  private

  def report_params
    params.require(:report).permit(:message)
  end
end
