
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  # This will not be matched by a "defp" pattern.
  def show(conn, %{"id" => id}) do
    user = Repo.get(User, id)
    render(conn, :show, user: user)
  end

  #ERROR: match
  defp show_private(conn, %{"id" => id}) do
    user = Repo.get(User, id)
    render(conn, :show, user: user)
  end
end
