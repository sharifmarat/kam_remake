unit KM_GameTypes;
{$I KaM_Remake.inc}
interface

type
  TKMGameMode = (
    gmSingle,
    gmCampaign,
    gmMulti,        //Different GIP, networking,
    gmMultiSpectate,
    gmMapEd,        //Army handling, lite updates,
    gmReplaySingle, //No input, different results screen to gmReplayMulti
    gmReplayMulti   //No input, different results screen to gmReplaySingle
    );

  TKMGameModeChangeEvent = procedure (aGameMode: TKMGameMode) of Object;

implementation

end.
