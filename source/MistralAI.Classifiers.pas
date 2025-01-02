unit MistralAI.Classifiers;

{-------------------------------------------------------------------------------

      Github repository : https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, System.NetEncoding, MistralAI.API.Params, MistralAI.API,
  MistralAI.Chat, MistralAI.Functions.Tools, MistralAI.Params.Core,
  MistralAI.Async.Support, MistralAI.Types;

type
  /// <summary>
  /// <c>TModerationParams</c> class to manage params for guardrails in a text.
  /// </summary>
  /// <remarks>
  /// The policy threshold is determined based on the optimal performance of oan internal test set.
  /// You can use the raw score or adjust the threshold according to your specific use cases.
  /// </remarks>
  TModerationParams = class(TJSONParam)
  public
    /// <summary>
    /// Text submitted for moderation
    /// </summary>
    /// <returns>
    /// The updated <c>TModerationParams</c> instance.
    /// </returns>
    function Input(const Value: string): TModerationParams; overload;
    /// <summary>
    /// Array of text submitted for moderation
    /// </summary>
    /// <returns>
    /// The updated <c>TModerationParams</c> instance.
    /// </returns>
    function Input(const Value: TArray<string>): TModerationParams; overload;
    /// <summary>
    /// Model for guardraile (e.g. mistral-moderation-latest)
    /// </summary>
    /// <returns>
    /// The updated <c>TModerationParams</c> instance.
    /// </returns>
    function Model(const Value: string): TModerationParams;
  end;

  /// <summary>
  /// <c>TModerationChatParams</c> class to manage params for guardrails in a chat generation.
  /// </summary>
  /// <remarks>
  /// The policy threshold is determined based on the optimal performance of oan internal test set.
  /// You can use the raw score or adjust the threshold according to your specific use cases.
  /// </remarks>
  TModerationChatParams = class(TJSONParam)
  public
    /// <summary>
    /// Provides the prompt(s) for the model to generate completions from, structured as a list of messages with roles (user, assistant, system) and content.
    /// </summary>
    /// <param name="Value">An array of <c>TChatMessagePayload</c> representing the messages in the conversation.</param>
    /// <returns>
    /// The updated <c>TModerationChatParams</c> instance.
    /// </returns>
    function Input(const Value: TArray<TChatMessagePayload>): TModerationChatParams;
    /// <summary>
    /// Model for guardraile (e.g. mistral-moderation-latest)
    /// </summary>
    /// <returns>
    /// The updated <c>TModerationChatParams</c> instance.
    /// </returns>
    function Model(const Value: string): TModerationChatParams;
  end;

  /// <summary>
  /// <c>TModerationCategories</c> class defines the categories for moderation checks.
  /// </summary>
  /// <remarks>
  /// This class provides a structured representation of the moderation categories
  /// used to evaluate the content against predefined guardrails. Each property corresponds
  /// to a specific moderation category and indicates whether the content violates that category.
  /// </remarks>
  TModerationCategories = class
  private
    FSexual: Boolean;
    FHate_and_discrimination: Boolean;
    FViolence_and_threats: Boolean;
    FDangerous_and_criminal_content: Boolean;
    FSelfharm: Boolean;
    FHealth: Boolean;
    FFinancial: Boolean;
    FLaw: Boolean;
    FPii: Boolean;
  public
    /// <summary>
    /// Indicates whether the content depicts or promotes explicit sexual material.
    /// </summary>
    /// <remarks>
    /// This includes content explicitly describing sexual activities, nudity, or solicitation for
    /// sexual purposes. Educational or medical content about sexual health in a non-explicit,
    /// informational context is generally exempted.
    /// </remarks>
    property Sexual: Boolean read FSexual write FSexual;
    /// <summary>
    /// Indicates whether the content expresses prejudice or advocates discrimination.
    /// </summary>
    /// <remarks>
    /// This category covers content that targets individuals or groups based on protected
    /// characteristics such as race, ethnicity, religion, gender, sexual orientation, or disability.
    /// </remarks>
    property Hate_and_discrimination: Boolean read FHate_and_discrimination write FHate_and_discrimination;
    /// <summary>
    /// Indicates whether the content glorifies, incites, or threatens physical violence.
    /// </summary>
    /// <remarks>
    /// This includes content that graphically describes injury or death, issues explicit threats,
    /// or provides instructions for carrying out violent acts.
    /// </remarks>
    property Violence_and_threats: Boolean read FViolence_and_threats write FViolence_and_threats;
    /// <summary>
    /// Indicates whether the content promotes dangerous or illegal activities.
    /// </summary>
    /// <remarks>
    /// This category includes guidance on creating weapons, encouragement of extreme risk-taking behaviors,
    /// and promotion of illegal activities such as fraud or drug trafficking.
    /// </remarks>
    property Dangerous_and_criminal_content: Boolean read FDangerous_and_criminal_content write FDangerous_and_criminal_content;
    /// <summary>
    /// Indicates whether the content promotes self-harm or related behaviors.
    /// </summary>
    /// <remarks>
    /// This includes encouragement or glorification of deliberate self-injury, suicide, eating disorders,
    /// or other self-destructive behaviors.
    /// </remarks>
    property Selfharm: Boolean read FSelfharm write FSelfharm;
    /// <summary>
    /// Indicates whether the content contains or elicits medical advice.
    /// </summary>
    /// <remarks>
    /// This applies to detailed or tailored medical advice that is not appropriate for non-expert contexts.
    /// </remarks>
    property Health: Boolean read FHealth write FHealth;
    /// <summary>
    /// Indicates whether the content contains or elicits financial advice.
    /// </summary>
    /// <remarks>
    /// This applies to detailed or tailored financial advice that may not align with ethical or legal standards.
    /// </remarks>
    property Financial: Boolean read FFinancial write FFinancial;
    /// <summary>
    /// Indicates whether the content contains or elicits legal advice.
    /// </summary>
    /// <remarks>
    /// This applies to detailed or tailored legal advice that should not be provided in a general context.
    /// </remarks>
    property Law: Boolean read FLaw write FLaw;
    /// <summary>
    /// Indicates whether the content involves personal identifying information (PII).
    /// </summary>
    /// <remarks>
    /// This category applies to content requesting, sharing, or attempting to elicit PII such as names,
    /// addresses, phone numbers, or financial account details.
    /// </remarks>
    property Pii: Boolean read FPii write FPii;
  end;

  /// <summary>
  /// <c>TModerationScores</c> class defines the scoring system for moderation categories.
  /// </summary>
  /// <remarks>
  /// This class provides a structured representation of the scores associated with each moderation category.
  /// Each property contains a numerical value indicating the likelihood that the content violates the corresponding category.
  /// </remarks>
  TModerationScores = class
  private
    FSexual: Double;
    FHate_and_discrimination: Double;
    FViolence_and_threats: Double;
    FDangerous_and_criminal_content: Double;
    FSelfharm: Double;
    FHealth: Double;
    FFinancial: Double;
    FLaw: Double;
    FPii: Double;
  public
    /// <summary>
    /// Numerical score indicating the likelihood of explicit sexual content.
    /// </summary>
    /// <remarks>
    /// Higher scores suggest a higher probability that the content explicitly depicts or promotes
    /// sexual activities, nudity, or solicitation for sexual purposes.
    /// </remarks>
    property Sexual: Double read FSexual write FSexual;
    /// <summary>
    /// Numerical score indicating the likelihood of hateful or discriminatory content.
    /// </summary>
    /// <remarks>
    /// Higher scores suggest a higher probability that the content targets individuals or groups based
    /// on protected characteristics such as race, ethnicity, religion, gender, or sexual orientation.
    /// </remarks>
    property Hate_and_discrimination: Double read FHate_and_discrimination write FHate_and_discrimination;
    /// <summary>
    /// Numerical score indicating the likelihood of violent or threatening content.
    /// </summary>
    /// <remarks>
    /// Higher scores suggest a higher probability that the content glorifies, incites, or threatens
    /// physical violence or provides instructions for violent acts.
    /// </remarks>
    property Violence_and_threats: Double read FViolence_and_threats write FViolence_and_threats;
    /// <summary>
    /// Numerical score indicating the likelihood of dangerous or illegal content.
    /// </summary>
    /// <remarks>
    /// Higher scores suggest a higher probability that the content promotes or provides instructions
    /// for illegal or haza
    property Dangerous_and_criminal_content: Double read FDangerous_and_criminal_content write FDangerous_and_criminal_content;
    /// <summary>
    /// Numerical score indicating the likelihood of content promoting self-harm.
    /// </summary>
    /// <remarks>
    /// Higher scores suggest a higher probability that the content encourages or glorifies
    /// deliberate self-injury, suicide, or other self-destructive behaviors.
    /// </remarks>
    property Selfharm: Double read FSelfharm write FSelfharm;
    /// <summary>
    /// Numerical score indicating the likelihood of content containing medical advice.
    /// </summary>
    /// <remarks>
    /// Higher scores suggest a higher probability that the content contains or attempts to elicit
    /// detailed or tailored medical advice.
    /// </remarks>
    property Health: Double read FHealth write FHealth;
    /// <summary>
    /// Numerical score indicating the likelihood of content containing financial advice.
    /// </summary>
    /// <remarks>
    /// Higher scores suggest a higher probability that the content contains or attempts to elicit
    /// detailed or tailored financial advice.
    /// </remarks>
    property Financial: Double read FFinancial write FFinancial;
    /// <summary>
    /// Numerical score indicating the likelihood of content containing legal advice.
    /// </summary>
    /// <remarks>
    /// Higher scores suggest a higher probability that the content contains or attempts to elicit
    /// detailed or tailored legal advice.
    /// </remarks>
    property Law: Double read FLaw write FLaw;
    /// <summary>
    /// Numerical score indicating the likelihood of content involving personal identifying information (PII).
    /// </summary>
    /// <remarks>
    /// Higher scores suggest a higher probability that the content requests, shares, or attempts
    /// to elicit personal information such as names, addresses, phone numbers, or financial details.
    /// </remarks>
    property Pii: Double read FPii write FPii;
  end;

  /// <summary>
  /// Represents a warning item in the moderation results, containing the category and its associated score.
  /// </summary>
  /// <remarks>
  /// This record is used to store information about a specific moderation category violation,
  /// including the category name and the numerical score indicating the likelihood of the violation.
  /// </remarks>
  TWarningItem = record
  private
    FCategory: string;
    FScore: Double;
  public
    /// <summary>
    /// Gets or sets the name of the moderation category.
    /// </summary>
    /// <value>
    /// A <c>string</c> representing the name of the category, such as "Sexual" or "Hate_and_discrimination."
    /// </value>
    property Category: string read FCategory write FCategory;
    /// <summary>
    /// Gets or sets the numerical score indicating the likelihood of a violation in the category.
    /// </summary>
    /// <value>
    /// A <c>Double</c> representing the likelihood score, where higher values indicate a greater likelihood of a violation.
    /// </value>
    property Score: Double read FScore write FScore;
    /// <summary>
    /// Creates a new instance of <c>TWarningItem</c> with the specified category and score.
    /// </summary>
    /// <param name="Category">
    /// The name of the moderation category.
    /// </param>
    /// <param name="Score">
    /// The numerical score associated with the category.
    /// </param>
    /// <returns>
    /// A new <c>TWarningItem</c> instance containing the specified category and score.
    /// </returns>
    class function Create(const Category: string; const Score: Double): TWarningItem; static;
  end;

  /// <summary>
  /// <c>TModerationResult</c> class represents the results of a moderation check.
  /// </summary>
  /// <remarks>
  /// This class contains information about the moderation categories and their associated scores.
  /// It provides a detailed breakdown of whether content violates predefined moderation categories
  /// and the likelihood of such violations.
  /// </remarks>
  TModerationResult = class
  strict private
    function GetWarning: Boolean;
  private
    FCategories: TModerationCategories;
    FCategory_scores: TModerationScores;
  public
    /// <summary>
    /// Retrieves an array of warning items for categories that have been flagged as violations.
    /// </summary>
    /// <remarks>
    /// This method analyzes the moderation results and returns a list of warning items,
    /// where each item contains the category name and its associated score. Only categories
    /// with a flagged status (true) in the moderation result are included in the returned array.
    /// </remarks>
    /// <returns>
    /// An array of <c>TWarningItem</c> instances, each representing a flagged moderation category
    /// along with its likelihood score.
    /// </returns>
    function Warnings: TArray<TWarningItem>;
    /// <summary>
    /// Contains the categories indicating whether the content violates specific moderation guardrails.
    /// </summary>
    /// <remarks>
    /// Each property within <c>TModerationCategories</c> indicates whether a particular category is violated.
    /// These categories include areas such as sexual content, hate speech, violence, and others.
    /// </remarks>
    property Categories: TModerationCategories read FCategories write FCategories;
    /// <summary>
    /// Contains the scores for each moderation category, indicating the likelihood of violations.
    /// </summary>
    /// <remarks>
    /// Each property within <c>TModerationScores</c> represents a numerical score. Higher scores suggest
    /// a greater probability that the content violates the corresponding category.
    /// </remarks>
    property Scores: TModerationScores read FCategory_scores write FCategory_scores;
    /// <summary>
    /// Returns true if at least one of the moderation categories is active.
    /// </summary>
    property Warning: Boolean read GetWarning;
    /// <summary>
    /// Destructor for the <c>TModerationResult</c> class.
    /// </summary>
    /// <remarks>
    /// Ensures proper cleanup of dynamically allocated resources, including the
    /// <c>Categories</c> and <c>Scores</c> objects.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// <c>TModeration</c> class represents the overall result of a moderation request.
  /// </summary>
  /// <remarks>
  /// This class provides a high-level summary of the moderation operation,
  /// including the request ID, the model used for moderation, and the detailed results
  /// for each category and score. It is the main structure returned by the moderation API.
  /// </remarks>
  TModeration = class
  private
    FId: string;
    FModel: string;
    FResults: TArray<TModerationResult>;
  public
    /// <summary>
    /// Unique identifier for the moderation request.
    /// </summary>
    /// <remarks>
    /// This property is useful for tracking or referencing specific moderation operations.
    /// </remarks>
    property Id: string read FId write FId;
    /// <summary>
    /// Name of the model used to process the moderation request.
    /// </summary>
    /// <remarks>
    /// Typically, this is the versioned name of the moderation model, such as
    /// "mistral-moderation-latest".
    /// </remarks>
    property Model: string read FModel write FModel;
    /// <summary>
    /// Array of detailed moderation results.
    /// </summary>
    /// <remarks>
    /// Each element in this array represents a separate result for a moderation check,
    /// containing both category flags and likelihood scores.
    /// </remarks>
    property Results: TArray<TModerationResult> read FResults write FResults;
    /// <summary>
    /// Destructor for the <c>TModeration</c> class.
    /// </summary>
    /// <remarks>
    /// Ensures proper cleanup of dynamically allocated resources, particularly the array
    /// of <c>TModerationResult</c> objects.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TModeration</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynModeration</c> type extends the <c>TAsynParams&lt;TModeration&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynModeration = TAsyncCallBack<TModeration>;

  /// <summary>
  /// <c>TClassifiersRoute</c> class provides an interface to interact with the MistralAI API for moderation tasks.
  /// </summary>
  /// <remarks>
  /// This class serves as a route handler for moderation-related operations, including both synchronous and asynchronous processing.
  /// It allows developers to send text or chat messages for moderation and receive structured feedback on their compliance with predefined guardrails.
  /// The available methods support various moderation scenarios, including text and conversational data.
  /// </remarks>
  TClassifiersRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Submit a text for moderation
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TModerationParams</c> parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns <c>TAsynModeration</c> to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// <para>
    /// The <c>CallBacks</c> function is invoked when the operation completes, either successfully or with an error.
    /// </para>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    /// MistralAI.AsynModeration(
    ///   procedure (Params: TModerationParams)
    ///   begin
    ///     // Define parameters
    ///   end,
    ///
    ///   function : TAsynModeration
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TModeration)
    ///        begin
    ///          // Handle the display
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncModeration(ParamProc: TProc<TModerationParams>; CallBacks: TFunc<TAsynModeration>);
    /// <summary>
    /// Submit a text in a chat generation system for moderation.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TModerationChatParams</c> parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns <c>TAsynModeration</c> to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// <para>
    /// The <c>CallBacks</c> function is invoked when the operation completes, either successfully or with an error.
    /// </para>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    /// MistralAI.AsynModerationChat(
    ///   procedure (Params: TModerationChatParams)
    ///   begin
    ///     // Define parameters
    ///   end,
    ///
    ///   function : TAsynModeration
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TModeration)
    ///        begin
    ///          // Handle the display
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncModerationChat(ParamProc: TProc<TModerationChatParams>; CallBacks: TFunc<TAsynModeration>);
    /// <summary>
    /// Submit a text for moderation
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TModerationParams</c> parameters.
    /// </param>
    /// <returns>
    /// A <c>TModeration</c> object containing the AudioToAudio result.
    /// </returns>
    /// <remarks>
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// //var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    /// var Value := MistralAI.Moderation(
    ///     procedure (Params: TModerationParams)
    ///     begin
    ///       // Define parameters
    ///     end;
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Moderation(ParamProc: TProc<TModerationParams>): TModeration;
    /// <summary>
    /// Submit a text in a chat generation system for moderation.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TModerationChatParams</c> parameters.
    /// </param>
    /// <returns>
    /// A <c>TModeration</c> object containing the AudioToAudio result.
    /// </returns>
    /// <remarks>
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// //var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    /// var Value := MistralAI.ModerationChat(
    ///     procedure (Params: TModerationChatParams)
    ///     begin
    ///       // Define parameters
    ///     end;
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function ModerationChat(ParamProc: TProc<TModerationChatParams>): TModeration;
  end;

implementation

{ TModerationParams }

function TModerationParams.Input(const Value: string): TModerationParams;
begin
  Result := TModerationParams(Add('input', Value));
end;

function TModerationParams.Input(
  const Value: TArray<string>): TModerationParams;
begin
  Result := TModerationParams(Add('input', Value));
end;

function TModerationParams.Model(const Value: string): TModerationParams;
begin
  Result := TModerationParams(Add('model', Value));
end;

{ TModerationResult }

destructor TModerationResult.Destroy;
begin
  if Assigned(FCategories) then
    FCategories.Free;
  if Assigned(FCategory_scores) then
    FCategory_scores.Free;
  inherited;
end;

function TModerationResult.GetWarning: Boolean;
begin
  Result := Categories.Sexual or Categories.FHate_and_discrimination or
    Categories.Violence_and_threats or Categories.Dangerous_and_criminal_content or
    Categories.Selfharm or Categories.Health or Categories.FFinancial or
    Categories.Law or Categories.Pii;
end;

function TModerationResult.Warnings: TArray<TWarningItem>;
begin
  if Categories.Sexual then
    Result := Result + [TWarningItem.Create(Classifiers[0], Scores.Sexual)];
  if Categories.Hate_and_discrimination then
    Result := Result + [TWarningItem.Create(Classifiers[1], Scores.Hate_and_discrimination)];
  if Categories.Violence_and_threats then
    Result := Result + [TWarningItem.Create(Classifiers[2], Scores.Violence_and_threats)];
  if Categories.Dangerous_and_criminal_content then
    Result := Result + [TWarningItem.Create(Classifiers[3], Scores.Dangerous_and_criminal_content)];
  if Categories.Selfharm then
    Result := Result + [TWarningItem.Create(Classifiers[4], Scores.Selfharm)];
  if Categories.Health then
    Result := Result + [TWarningItem.Create(Classifiers[5], Scores.Health)];
  if Categories.Financial then
    Result := Result + [TWarningItem.Create(Classifiers[6], Scores.Financial)];
  if Categories.Law then
    Result := Result + [TWarningItem.Create(Classifiers[7], Scores.Law)];
  if Categories.Pii then
    Result := Result + [TWarningItem.Create(Classifiers[8], Scores.Pii)];
end;

{ TModeration }

destructor TModeration.Destroy;
begin
  for var Item in FResults do
    Item.Free;
  inherited;
end;

{ TClassifiersRoute }

procedure TClassifiersRoute.AsyncModeration(ParamProc: TProc<TModerationParams>;
  CallBacks: TFunc<TAsynModeration>);
begin
  with TAsyncCallBackExec<TAsynModeration, TModeration>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModeration
      begin
        Result := Self.Moderation(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TClassifiersRoute.AsyncModerationChat(
  ParamProc: TProc<TModerationChatParams>; CallBacks: TFunc<TAsynModeration>);
begin
  with TAsyncCallBackExec<TAsynModeration, TModeration>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModeration
      begin
        Result := Self.ModerationChat(ParamProc);
      end);
  finally
    Free;
  end;
end;

function TClassifiersRoute.Moderation(
  ParamProc: TProc<TModerationParams>): TModeration;
begin
  Result := API.Post<TModeration, TModerationParams>('moderations', ParamProc);
end;

function TClassifiersRoute.ModerationChat(
  ParamProc: TProc<TModerationChatParams>): TModeration;
begin
  Result := API.Post<TModeration, TModerationChatParams>('chat/moderations', ParamProc);
end;

{ TModerationChatParams }

function TModerationChatParams.Input(
  const Value: TArray<TChatMessagePayload>): TModerationChatParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TModerationChatParams(Add('input', JSONArray));
end;

function TModerationChatParams.Model(
  const Value: string): TModerationChatParams;
begin
  Result := TModerationChatParams(Add('model', Value));
end;

{ TWarningItem }

class function TWarningItem.Create(const Category: string;
  const Score: Double): TWarningItem;
begin
  Result.Category := Category;
  Result.Score := Score;
end;

end.
