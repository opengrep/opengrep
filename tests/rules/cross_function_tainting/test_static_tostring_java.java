@Controller
public class FormController {
  public void processForm(HttpServletRequest request) {
    String[] values = request.getParameterValues("tags");
    // ruleid: test-tostring-taint
    logger.info("Form tags: " + Arrays.toString(values));
  }
}
