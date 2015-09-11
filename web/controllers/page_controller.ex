defmodule SignalExample.PageController do
  use SignalExample.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
