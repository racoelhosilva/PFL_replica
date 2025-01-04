% config_name(+GameConfig, +Player, -Name)
% Returns the name of the given player in the given game configuration.
config_name(game_config(_, player_info(Name, _), _), white, Name).
config_name(game_config(_, _, player_info(Name, _)), black, Name).

% config_difficulty(+GameConfig, +Player, -Difficulty)
% Returns the difficulty of the given player in the given game configuration.
config_difficulty(game_config(_, player_info(_, Difficulty), _), white, Difficulty).
config_difficulty(game_config(_, _, player_info(_, Difficulty)), black, Difficulty).
