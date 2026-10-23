//ERROR:
case class Youtube(channelId: String)
    derives CanEqual

//OK:
case class Twitch(channelId: String)
