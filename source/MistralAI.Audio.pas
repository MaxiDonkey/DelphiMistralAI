unit MistralAI.Audio;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.Classes, System.SysUtils, REST.JsonReflect, REST.Json.Types, System.Net.Mime,
  System.Threading, MistralAI.API.Params, MistralAI.API, MistralAI.Async.Support,
  MistralAI.Types, MistralAI.Async.Promise;

type
  /// <summary>
  /// Represents the parameter builder for configuring an audio transcription request.
  /// </summary>
  /// <remarks>
  /// Use <c>TAudioTranscriptionParams</c> to set various input fields such as file path or URL, model name,
  /// language, and timestamp options. The configured instance is submitted to the transcription endpoint to
  /// obtain speech-to-text results from an audio source.
  /// </remarks>
  TAudioTranscriptionParams = class(TMultipartFormData)

    /// <summary>
    /// Sets the audio input for transcription via a local file path or a URL.
    /// </summary>
    /// <param name="Value">
    /// A file path (e.g., 'C:\audio\clip.mp3') or a public URL (e.g., 'https://example.com/audio.wav').
    /// </param>
    /// <returns>
    /// The current <c>TAudioTranscriptionParams</c> instance for fluent configuration.
    /// </returns>
    /// <exception cref="Exception">
    /// Raised if the local file does not exist or cannot be opened.
    /// </exception>
    /// <remarks>
    /// The method will determine whether the value is a URL or a file path and upload the audio accordingly.
    /// </remarks>
    function FileUrl(const Value: string): TAudioTranscriptionParams;

    /// <summary>
    /// Specifies the model to be used for transcription.
    /// </summary>
    /// <param name="Value">
    /// The model identifier string (e.g., 'voxtral-mini-2507').
    /// </param>
    /// <returns>
    /// The current <c>TAudioTranscriptionParams</c> instance for fluent configuration.
    /// </returns>
    function Model(const Value: string): TAudioTranscriptionParams;

    /// <summary>
    /// Defines the spoken language of the audio input to improve recognition accuracy.
    /// </summary>
    /// <param name="Value">
    /// A language code following the BCP 47 format (e.g., 'en', 'fr', 'es').
    /// </param>
    /// <returns>
    /// The current <c>TAudioTranscriptionParams</c> instance for fluent configuration.
    /// </returns>
    /// <remarks>
    /// If not specified, the transcription service attempts automatic language detection.
    /// </remarks>
    function Language(const Value: string): TAudioTranscriptionParams;

    /// <summary>
    /// Enables timestamp annotations in the transcription output.
    /// </summary>
    /// <param name="Value">
    /// A string value indicating the level of timestamp granularity. Currently, only 'segment' is supported.
    /// </param>
    /// <returns>
    /// The current <c>TAudioTranscriptionParams</c> instance for fluent configuration.
    /// </returns>
    /// <remarks>
    /// Timestamp granularity allows you to track when each segment of speech starts and ends.
    /// </remarks>
    function TimestampGranularities(const Value: string): TAudioTranscriptionParams;

    /// <summary>
    /// Creates a new instance of <c>TAudioTranscriptionParams</c>.
    /// </summary>
    /// <remarks>
    /// Automatically initializes the underlying multipart form structure.
    /// </remarks>
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// Represents a single transcribed segment from an audio file.
  /// </summary>
  /// <remarks>
  /// Each instance of <c>TAudioSegment</c> contains the textual content of a portion of audio along with
  /// its start and end timestamps, allowing you to reconstruct the timing of speech within the original audio.
  /// </remarks>
  TAudioSegment = class
  private
    FText: string;
    FStart: Double;
    FEnd: Double;
  public
    /// <summary>
    /// The transcribed text content for this audio segment.
    /// </summary>
    /// <value>
    /// A string representing the portion of speech that occurred between <c>Start</c> and <c>End</c>.
    /// </value>
    property Text: string read FText write FText;

    /// <summary>
    /// The starting time of the segment, in seconds.
    /// </summary>
    /// <value>
    /// A <c>Double</c> value indicating the offset from the beginning of the audio file where the speech in this segment starts.
    /// </value>
    property Start: Double read FStart write FStart;

    /// <summary>
    /// The ending time of the segment, in seconds.
    /// </summary>
    /// <value>
    /// A <c>Double</c> value indicating the offset from the beginning of the audio file where the speech in this segment ends.
    /// </value>
    property &End: Double read FEnd write FEnd;
  end;

  /// <summary>
  /// Represents usage statistics related to an audio transcription request.
  /// </summary>
  /// <remarks>
  /// <c>TAudioUsage</c> provides detailed metrics about the resources consumed during the transcription
  /// process, including token counts and the total duration of the audio input.
  /// </remarks>
  TAudioUsage = class
  private
    [JsonNameAttribute('prompt_audio_seconds')]
    FPromptAudioSeconds: Int64;
    [JsonNameAttribute('prompt_tokens')]
    FPromptTokens: Int64;
    [JsonNameAttribute('total_tokens')]
    FTotalTokens: Int64;
    [JsonNameAttribute('completion_tokens')]
    FCompletionTokens: Int64;
  public
    /// <summary>
    /// Total duration of the input audio in seconds.
    /// </summary>
    /// <value>
    /// An integer representing the number of seconds of audio processed during the transcription.
    /// </value>
    property PromptAudioSeconds: Int64 read FPromptAudioSeconds write FPromptAudioSeconds;

    /// <summary>
    /// Total duration of the input audio in seconds.
    /// </summary>
    /// <value>
    /// An integer representing the number of seconds of audio processed during the transcription.
    /// </value>
    property PromptTokens: Int64 read FPromptTokens write FPromptTokens;

    /// <summary>
    /// Total number of tokens involved in the transcription process.
    /// </summary>
    /// <value>
    /// The combined count of prompt and completion tokens.
    /// </value>
    property TotalTokens: Int64 read FTotalTokens write FTotalTokens;

    /// <summary>
    /// Number of tokens generated as part of the transcription output.
    /// </summary>
    /// <value>
    /// The token count corresponding to the textual content produced by the transcription model.
    /// </value>
    property CompletionTokens: Int64 read FCompletionTokens write FCompletionTokens;
  end;

  /// <summary>
  /// Represents the result of an audio transcription request.
  /// </summary>
  /// <remarks>
  /// <c>TAudioTranscription</c> encapsulates the output of a transcription task, including the full transcribed text,
  /// detected language, model used, timestamped segments, and usage statistics. It is returned as the response
  /// object from the audio transcription API endpoint.
  /// </remarks>
  TAudioTranscription = class(TJSONFingerprint)
  private
    FModel: string;
    FText: string;
    FLanguage: string;
    FSegments : TArray<TAudioSegment>;
    FUsage: TAudioUsage;
  public
    /// <summary>
    /// The identifier of the model used for the transcription.
    /// </summary>
    /// <value>
    /// A string representing the name or version of the transcription model (e.g., 'voxtral-mini-2507').
    /// </value>
    property Model: string read FModel write FModel;

    /// <summary>
    /// The full transcribed text of the audio input.
    /// </summary>
    /// <value>
    /// A string containing the entire speech-to-text output as a single plain text block.
    /// </value>
    property Text: string read FText write FText;

    /// <summary>
    /// The detected or specified language of the transcription.
    /// </summary>
    /// <value>
    /// A BCP 47 language code string (e.g., 'en', 'fr', 'de').
    /// </value>
    property Language: string read FLanguage write FLanguage;

    /// <summary>
    /// The list of timestamped segments corresponding to speech intervals.
    /// </summary>
    /// <value>
    /// An array of <c>TAudioSegment</c> objects, each representing a portion of speech with timing metadata.
    /// </value>
    /// <remarks>
    /// This property is populated when <c>timestamp_granularities = 'segment'</c> is used during the request.
    /// </remarks>
    property Segments : TArray<TAudioSegment> read FSegments write FSegments;

    /// <summary>
    /// Detailed usage statistics for the transcription operation.
    /// </summary>
    /// <value>
    /// A <c>TAudioUsage</c> object providing information such as token counts and audio duration.
    /// </value>
    property Usage: TAudioUsage read FUsage write FUsage;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the asynchronous callback structure for audio transcription operations.
  /// </summary>
  /// <remarks>
  /// <c>TAsyncAudioTranscription</c> is a type alias for <c>TAsyncCallback&lt;TAudioTranscription&gt;</c>.
  /// It allows you to assign lifecycle event handlers such as <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c>
  /// when performing non-blocking transcription requests.
  /// This is useful for integrating event-driven logic into your application when working with
  /// <c>TAudioRoute.AsyncTranscription</c> to process audio asynchronously without blocking the main thread.
  /// </remarks>
  TAsyncAudioTranscription = TAsyncCallback<TAudioTranscription>;

  /// <summary>
  /// Represents the promise-based interface for audio transcription operations.
  /// </summary>
  /// <remarks>
  /// <c>TPromiseAudioTranscription</c> is a type alias for <c>TPromiseCallback&lt;TAudioTranscription&gt;</c>.
  /// It provides a structured way to handle asynchronous transcription results using a promise-like API,
  /// allowing chaining of <c>OnSuccess</c> and <c>OnError</c> handlers.
  /// This type is typically used with <c>TAudioRoute.AsyncAwaitTranscription</c> for workflows where you want
  /// to react to the transcription result once it's completed, without relying on event callbacks.
  /// </remarks>
  TPromiseAudioTranscription = TPromiseCallback<TAudioTranscription>;

  TAudioRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Submits an asynchronous audio transcription request and returns a promise for its result.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that configures the <c>TAudioTranscriptionParams</c> instance used to define the transcription request,
    /// such as the audio source, model selection, language, or timestamp settings.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseAudioTranscription</c> instance, allowing you to assign
    /// <c>OnSuccess</c> and <c>OnError</c> handlers for promise-style chaining.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TAudioTranscription&gt;</c> that resolves with the transcription result or
    /// rejects with an exception if the operation fails.
    /// </returns>
    /// <remarks>
    /// This method enables a modern, promise-based approach to asynchronous programming, allowing you to compose
    /// follow-up logic once the transcription completes. It is especially useful in functional or await-style workflows.
    /// The transcription executes on a background thread. No UI blocking occurs.
    /// </remarks>
    function AsyncAwaitTranscription(
      const ParamProc: TProc<TAudioTranscriptionParams>;
      const Callbacks: TFunc<TPromiseAudioTranscription> = nil): TPromise<TAudioTranscription>;

    /// <summary>
    /// Executes a synchronous audio transcription request using the provided parameter configuration.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that receives a <c>TAudioTranscriptionParams</c> instance to configure the transcription request,
    /// including fields such as file path or URL, model, language, and timestamp options.
    /// </param>
    /// <returns>
    /// A <c>TAudioTranscription</c> object containing the result of the transcription,
    /// including full text, segments (if requested), detected language, and usage metadata.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking call. It should be used when you require an immediate result and
    /// can afford to wait until the request is fully processed.
    /// If an error occurs (e.g., invalid parameters, network failure, API error), an exception is raised.
    /// </remarks>
    function Transcription(const ParamProc: TProc<TAudioTranscriptionParams>): TAudioTranscription;

    /// <summary>
    /// Performs an asynchronous audio transcription request using callback-style event handlers.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the <c>TAudioTranscriptionParams</c> object, such as specifying the audio file URL or path,
    /// the transcription model, language, or timestamp granularity.
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a <c>TAsyncAudioTranscription</c> instance, where you can assign handlers such as
    /// <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> to respond to different stages of the transcription lifecycle.
    /// </param>
    /// <remarks>
    /// This method is non-blocking and executes the transcription request on a background thread.
    /// It is suitable for UI applications or services that require reactive, event-driven processing without blocking the main thread.
    /// Use this method when you want fine-grained control over the asynchronous execution flow through explicit callbacks.
    /// </remarks>
    procedure AsyncTranscription(const ParamProc: TProc<TAudioTranscriptionParams>;
      const Callbacks: TFunc<TAsyncAudioTranscription>);
  end;

implementation

{ TAudioTranscriptionParams }

constructor TAudioTranscriptionParams.Create;
begin
  inherited Create(True);
end;

function TAudioTranscriptionParams.FileUrl(
  const Value: string): TAudioTranscriptionParams;
begin
  if Value.StartsWith('http') then
    AddField('file_url', Value)
  else
    begin
      if not FileExists(Value) then
        raise Exception.CreateFmt('File not found: %s', [Value]);
      var AudioStream := TFileStream.Create(Value, fmOpenRead or fmShareDenyWrite);
      AudioStream.Position := 0;
      try
        {$IF RTLVersion > 35.0}
        AddStream('file', AudioStream, True, ExtractFileName(Value));
        {$ELSE}
        AddStream('file', Stream, FileName);
        {$ENDIF}
      finally
      end;
    end;
  Result := Self;
end;

function TAudioTranscriptionParams.Language(
  const Value: string): TAudioTranscriptionParams;
begin
  AddField('language', Value);
  Result := Self;
end;

function TAudioTranscriptionParams.Model(
  const Value: string): TAudioTranscriptionParams;
begin
  AddField('model', Value);
  Result := Self;
end;

function TAudioTranscriptionParams.TimestampGranularities(
  const Value: string): TAudioTranscriptionParams;
begin
  AddField('timestamp_granularities', Value);
  Result := Self;
end;

{ TAudioTranscription }

destructor TAudioTranscription.Destroy;
begin
  for var Item in FSegments do
    Item.Free;
  if Assigned(FUsage) then
    FUsage.Free;
  inherited;
end;

{ TAudioRoute }

function TAudioRoute.AsyncAwaitTranscription(
  const ParamProc: TProc<TAudioTranscriptionParams>;
  const Callbacks: TFunc<TPromiseAudioTranscription>): TPromise<TAudioTranscription>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAudioTranscription>(
    procedure(const CallbackParams: TFunc<TAsyncAudioTranscription>)
    begin
      AsyncTranscription(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

procedure TAudioRoute.AsyncTranscription(
  const ParamProc: TProc<TAudioTranscriptionParams>;
  const Callbacks: TFunc<TAsyncAudioTranscription>);
begin
  with TAsyncCallBackExec<TAsyncAudioTranscription, TAudioTranscription>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAudioTranscription
      begin
        Result := Self.Transcription(ParamProc);
      end);
  finally
    Free;
  end;
end;

function TAudioRoute.Transcription(
  const ParamProc: TProc<TAudioTranscriptionParams>): TAudioTranscription;
begin
  Result := API.PostForm<TAudioTranscription, TAudioTranscriptionParams>('audio/transcriptions', ParamProc);
end;

end.
