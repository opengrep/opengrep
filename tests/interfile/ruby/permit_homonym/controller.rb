class Controller
  def create
    @report = Report.new(report_params)
    # ruleid: test-permit-homonym
    sink(@report)
  end

  private

  def report_params
    params.require(:report).permit(:message)
  end
end
