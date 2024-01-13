-ifndef(chat_app).

-define(LOGERROR(Text),           logger:error(Text)).
-define(LOGERROR(Text, Params),   logger:error(Text, Params)).
-define(LOGWARNING(Text),         logger:warning(Text)).
-define(LOGWARNING(Text, Params), logger:warning(Text, Params)).
-define(LOGINFO(Text),            logger:info(Text)).
-define(LOGINFO(Text, Params),    logger:info(Text, Params)).
-define(LOGDEBUG(Text),           logger:debug(Text)).
-define(LOGDEBUG(Text, Params),   logger:debug(Text, Params)).

-endif.