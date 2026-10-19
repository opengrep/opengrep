case class Youtube(channelId: String, liveVideoId: Option[String])
    derives Eq:
  def fullUrl = s"https://www.youtube.com/channel/$channelId/live"
