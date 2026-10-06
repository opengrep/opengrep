# A block applied where it is used reads the receiver's fields there: one the
# method set itself, and one a helper method it calls memoises.
class RunnersController
  def update
    @runner = Ci::Runner.find(params[:id])
    if Ci::UpdateRunnerService.new(@runner).update(runner_params)
      respond_to do |format|
        # ruleid: lambda-sig-ruby-ivar-block
        format.html { redirect_to admin_runner_path(@runner) }
      end
    end
  end
end

class KeysController
  def destroy
    key = user.keys.find(params[:id])
    respond_to do |format|
      # ruleid: lambda-sig-ruby-ivar-block
      format.html { redirect_to keys_path(user) }
    end
  end

  def show
    respond_to do |format|
      # ok: lambda-sig-ruby-ivar-block
      format.html { redirect_to keys_path(label) }
    end
  end

  def user
    @user ||= User.find_by!(username: params[:user_id])
  end

  def label
    @label ||= "keys"
  end
end
