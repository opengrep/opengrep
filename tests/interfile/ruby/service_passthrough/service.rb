class CreateService
  def initialize(user, data)
    @user = user
    @data = data
  end

  def execute(group)
    @data
  end
end
