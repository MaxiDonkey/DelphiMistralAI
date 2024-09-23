unit MistralAI.Codestral;


{-------------------------------------------------------------------------------

   **Important Notice**

   To utilize the classes managing the Codestral function, you are required to
   create a new KEY on the Mistral.ai website. Please note that obtaining this
   key necessitates providing a valid phone number.

   https://console.mistral.ai/codestral

      Github repository : https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

-------------------------------------------------------------------------------}


interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  System.Threading, MistralAI.API.Params, MistralAI.API, MistralAI.Async.Support,
  MistralAI.Params.Core;

type
  /// <summary>
  /// Represents the different reasons why the processing of a request can terminate.
  /// </summary>
  TCodestralFinishReason = (
    /// <summary>
    /// API returned complete model output
    /// </summary>
    stop,
    /// <summary>
    /// Incomplete model output due to max_tokens parameter or token limit
    /// </summary>
    length_limite,
    /// <summary>
    /// model_length
    /// </summary>
    model_length,
    /// <summary>
    /// An error was encountered while processing the request
    /// </summary>
    error);

  /// <summary>
  /// Provides helper methods for the TCodestralFinishReason enumeration.
  /// </summary>
  TCodestralFinishReasonHelper = record helper for TCodestralFinishReason
    /// <summary>
    /// Returns the string representation of the TCodestralFinishReason value.
    /// </summary>
    function ToString: string;
    /// <summary>
    /// Creates a TCodestralFinishReason value from a given string.
    /// </summary>
    /// <param name="Value">The string representation of the TCodestralFinishReason.</param>
    /// <returns>A TCodestralFinishReason corresponding to the provided string.</returns>
    class function Create(const Value: string): TCodestralFinishReason; static;
  end;

  /// <summary>
  /// Interceptor class for converting and reverting TCodestralFinishReason values to and from strings in JSON.
  /// </summary>
  TCodestralFinishReasonInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts a TCodestralFinishReason value to its string representation for JSON serialization.
    /// </summary>
    /// <param name="Data">The object containing the field to be converted.</param>
    /// <param name="Field">The name of the field to be converted.</param>
    /// <returns>The string representation of the TCodestralFinishReason value.</returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Reverts a string representation back to a TCodestralFinishReason value during JSON deserialization.
    /// </summary>
    /// <param name="Data">The object containing the field to be reverted.</param>
    /// <param name="Field">The name of the field to be reverted.</param>
    /// <param name="Arg">
    /// The string representation of the TCodestralFinishReason value.
    ///</param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// The <c>TCodestralParams</c> class represents the set of parameters used to configure the behavior and output generation of the Codestral model.
  /// </summary>
  /// <remarks>
  /// This class allows you to define various settings that control how the model generates completions, including which model to use, the temperature of the output,
  /// and how many tokens to generate. By using this class, you can customize the model's response and fine-tune its behavior to suit the specific requirements of your application.
  /// <para>
  /// It inherits from <c>TJSONParam</c>, which provides methods for handling and serializing the parameters as JSON, enabling easy integration
  /// with JSON-based APIs. The parameters include options for setting prompts, suffixes, streaming preferences, and random seeds, among others.
  /// </para>
  /// <code>
  /// var
  ///   Params: TCodestralParams;
  /// begin
  ///   Params := TCodestralParams.Create
  ///     .Model('codestral-latest')
  ///     .Prompt('Generate a code snippet')
  ///     .MaxTokens(100)
  ///     .Temperature(0.7)
  ///     .TopP(0.9)
  ///     .Stop(['\n\n'])
  ///     .Stream(True);          // or Stream();
  /// end;
  /// </code>
  /// This example demonstrates how to instantiate and configure a <c>TCodestralParams</c> object to generate a code snippet using the Codestral model.
  /// </remarks>
  TCodestralParams = class(TJSONParam)
    /// <summary>
    /// Sets the text or code to be completed by the model.
    /// </summary>
    /// <param name="Value">
    /// The prompt text or code that the model should complete.
    /// This parameter is required and serves as the starting point for the model's generation.
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    /// <remarks>
    /// The <c>Prompt</c> parameter is mandatory and provides the initial context that the model will use to generate a response.
    /// </remarks>
    function Prompt(const Value: string): TCodestralParams;
    /// <summary>
    /// Sets an optional suffix that provides additional context for the model.
    /// If both a prompt and a suffix are provided, the model will generate text to fill the space between them.
    /// </summary>
    /// <param name="Value">
    /// The suffix text that follows the content generated by the model.
    /// This helps guide the model on how to conclude the generation.
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    function Suffix(const Value: string): TCodestralParams;
    /// <summary>
    /// Specifies the identifier of the model to use.
    /// Currently compatible with "codestral-2405" or "codestral-latest".
    /// </summary>
    /// <param name="Value">
    /// The model ID to be used for the completion.
    /// Ensure that the specified model is supported and correctly spelled.
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This parameter is required and determines which model will process the request.
    /// </remarks>
    function Model(const Value: string): TCodestralParams;
    /// <summary>
    /// Sets the sampling temperature to use for the model's output.
    /// Higher values like 0.8 make the output more random, while lower values like 0.2 make it more focused and deterministic.
    /// </summary>
    /// <param name="Value">
    /// The temperature value between 0.0 and 1.0. Default is 0.7.
    /// A temperature of 0 makes the model deterministic, while a temperature of 1 allows for maximum creativity.
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    function Temperature(const Value: Single = 0.7): TCodestralParams;
    /// <summary>
    /// Sets the nucleus sampling probability mass for the model (Top-p).
    /// For example, 0.1 means only the tokens comprising the top 10% probability mass are considered.
    /// </summary>
    /// <param name="Value">
    /// The <c>top_p</c> value between 0.0 and 1.0. Default is 1.
    /// Lower values limit the model to consider only the most probable options.
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    function TopP(const Value: Single = 1): TCodestralParams;
    /// <summary>
    /// Sets the maximum number of tokens to generate in the completion.
    /// The total token count of your prompt plus <c>max_tokens</c> cannot exceed the model's context length.
    /// </summary>
    /// <param name="Value">
    /// The maximum number of tokens to generate.
    /// Choose an appropriate value based on your prompt length to avoid exceeding the model's limit.
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    function MaxTokens(const Value: Integer): TCodestralParams;
    /// <summary>
    /// Sets the minimum number of tokens to generate in the completion.
    /// The model will continue generating tokens until this minimum is reached, unless a stop sequence is encountered.
    /// </summary>
    /// <param name="Value">
    /// The minimum number of tokens the model should generate.
    /// Use this parameter to ensure a response of a certain minimum length.
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    function MinTokens(const Value: Integer): TCodestralParams;
    /// <summary>
    /// Specifies whether to stream back partial progress as server-sent events (SSE).
    /// If <c>true</c>, tokens are sent as they become available.
    /// If <c>false</c>, the server will hold the request open until timeout or completion.
    /// </summary>
    /// <param name="Value">
    /// A boolean value indicating whether to enable streaming. Default is <c>true</c>, meaning streaming is enabled by default.
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    function Stream(const Value: Boolean = True): TCodestralParams;
    /// <summary>
    /// Sets the seed for random sampling. If specified, different calls with the same seed will produce deterministic results.
    /// </summary>
    /// <param name="Value">
    /// The seed value to use for random sampling.
    /// This can be useful for reproducing results during debugging or testing.
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    function RandomSeed(const Value: Integer): TCodestralParams;
    /// <summary>
    /// Sets the tokens or sequences that will stop the generation process when detected.
    /// </summary>
    /// <param name="Value">
    /// An array of strings representing the stop tokens or sequences.
    /// Default is an empty array, meaning no specific stop sequences are defined.
    /// Use this parameter to indicate when the model should stop generating, such as specifying a sequence like "\n\n".
    /// </param>
    /// <returns>
    /// The updated <c>TCodestralParams</c> instance.
    /// </returns>
    function Stop(const Value: TArray<string> = []): TCodestralParams;
    /// <summary>
    /// Initializes a new instance of the TCodestralParams class with default values.
    /// </summary>
    constructor Create; override;
  end;

  /// <summary>
  /// Represents the token usage statistics for a request, including the number of tokens used in the prompt,
  /// the completion, and the total number of tokens consumed.
  /// </summary>
  /// <remarks>
  /// The <c>TCodestralUsage</c> class provides information on the number of tokens utilized during a request.
  /// This data is essential for understanding the token cost associated with a request, particularly in contexts
  /// where token-based billing is employed, or for monitoring the model's behavior in terms of input and output sizes.
  /// </remarks>
  TCodestralUsage = class
  private
    [JsonNameAttribute('prompt_tokens')]
    FPrompt_tokens: Int64;
    [JsonNameAttribute('completion_tokens')]
    FCompletion_tokens: Int64;
    [JsonNameAttribute('total_tokens')]
    FTotal_tokens: Int64;
  public
    /// <summary>
    /// The number of tokens consumed by the prompt in the request.
    /// </summary>
    /// <remarks>
    /// This property indicates how many tokens were used for the initial input prompt sent to the model.
    /// It helps to assess the portion of the token budget consumed by the user's input or system instructions.
    /// </remarks>
    property PromptTokens: Int64 read FPrompt_tokens write FPrompt_tokens;
    /// <summary>
    /// The number of tokens consumed by the completion generated by the model.
    /// </summary>
    /// <remarks>
    /// The <c>CompletionTokens</c> property tracks the number of tokens used in the model's response.
    /// This is useful for analyzing the token cost of the model's output in relation to the input size.
    /// </remarks>
    property CompletionTokens: Int64 read FCompletion_tokens write FCompletion_tokens;
    /// <summary>
    /// The total number of tokens used for the entire request, including both the prompt and the completion.
    /// </summary>
    /// <remarks>
    /// The <c>TotalTokens</c> property provides the cumulative token usage for a single request,
    /// combining both prompt and completion tokens. This value is crucial for monitoring overall token consumption
    /// and ensuring compliance with token limits or pricing considerations.
    /// </remarks>
    property TotalTokens: Int64 read FTotal_tokens write FTotal_tokens;
  end;

  /// <summary>
  /// Represents a message exchanged in a conversation, containing the role of the sender and the message content.
  /// </summary>
  /// <remarks>
  /// The <c>TCodestralMessage</c> class captures the essential details of a message in a chat interaction,
  /// including the role of the sender (e.g., user or assistant) and the content of the message.
  /// This class is fundamental for managing and interpreting the flow of a conversation, providing context on who sent
  /// the message and what was communicated.
  /// </remarks>
  TCodestralMessage = class
  private
    [JsonNameAttribute('role')]
    FRole: string;
    [JsonNameAttribute('content')]
    FContent: string;
  public
    /// <summary>
    /// The role of the author of this message.
    /// </summary>
    /// <remarks>
    /// The <c>Role</c> property specifies the participant who authored the message. It typically has values like "user" for user input
    /// or "assistant" for responses generated by the AI. This property helps in distinguishing the source of the message within the conversation.
    /// </remarks>
    property Role: string read FRole write FRole;
    /// <summary>
    /// The content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property holds the actual message text. This can be a user query, an AI-generated response, or other relevant information
    /// exchanged during the conversation.
    /// </remarks>
    property Content: string read FContent write FContent;
  end;

  /// <summary>
  /// Represents a single completion option generated by the AI model during a chat or completion interaction.
  /// </summary>
  /// <remarks>
  /// The <c>TCodestralChoices</c> class encapsulates the details of a single choice made by the AI model.
  /// Each instance of this class represents one possible response that the model could generate.
  /// This includes:
  /// - An index indicating the position of this choice among multiple options.
  /// - A complete message response generated by the model.
  /// - Optional deltas representing partial responses in the case of streamed outputs.
  /// - The reason why the model stopped generating tokens.
  /// This class is useful when evaluating multiple response options or handling streaming outputs that build over time.
  /// </remarks>
  TCodestralChoices = class
  private
    [JsonNameAttribute('index')]
    FIndex: Int64;
    [JsonNameAttribute('message')]
    FMessage: TCodestralMessage;
    [JsonReflectAttribute(ctString, rtString, TCodestralFinishReasonInterceptor)]
    FFinish_reason: TCodestralFinishReason;
    [JsonNameAttribute('delta')]
    FDelta: TCodestralMessage;
  public
    /// <summary>
    /// Represents the position of this choice within the list of available choices generated by the model.
    /// </summary>
    /// <remarks>
    /// The <c>Index</c> property identifies the index of this particular choice in the set of choices provided by the model.
    /// This helps differentiate between multiple possible responses generated during a completion.
    /// </remarks>
    property Index: Int64 read FIndex write FIndex;
    /// <summary>
    /// The full completion message generated by the AI model based on the provided input or conversation context.
    /// </summary>
    /// <remarks>
    /// The <c>Message</c> property contains the complete message that represents the model's response.
    /// This is typically a detailed reply generated in response to a user prompt or system directive.
    /// </remarks>
    property Message: TCodestralMessage read FMessage write FMessage;
    /// <summary>
    /// A partial message (delta) generated by the model during streaming responses.
    /// </summary>
    /// <remarks>
    /// The <c>Delta</c> property holds incremental content provided when the model streams its response.
    /// This allows for progressively building the final response before it is fully completed.
    /// </remarks>
    property Delta: TCodestralMessage read FDelta write FDelta;
    /// <summary>
    /// Indicates the reason why the model stopped generating tokens.
    /// </summary>
    /// <remarks>
    /// The <c>FinishReason</c> property describes why the generation process ceased. This could be due to reaching a natural end of the response,
    /// hitting a specified stop sequence, or encountering a model constraint like a token limit.
    /// </remarks>
    property FinishReason: TCodestralFinishReason read FFinish_reason write FFinish_reason;
    /// <summary>
    /// Destructor to release resources held by the <c>TCodestralChoices</c> instance.
    /// </summary>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a codestral completion response generated by an AI model, containing metadata,
  /// generated choices, and usage statistics.
  /// </summary>
  /// <remarks>
  /// The <c>TCodestral</c> class encapsulates the results of a completion request made to an AI model.
  /// It includes details such as a unique identifier, the model used, when the completion was created,
  /// the choices generated by the model, and usage statistics.
  /// This class is essential for managing and understanding the results of AI-driven completions,
  /// as well as tracking the underlying usage and response characteristics.
  /// </remarks>
  TCodestral = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('created')]
    FCreated: Int64;
    [JsonNameAttribute('model')]
    FModel: string;
    [JsonNameAttribute('choices')]
    FChoices: TArray<TCodestralChoices>;
    [JsonNameAttribute('usage')]
    FUsage: TCodestralUsage;
  public
    /// <summary>
    /// A unique identifier for the codestral completion.
    /// </summary>
    /// <remarks>
    /// The <c>Id</c> property stores a unique string that identifies the specific codestral completion request.
    /// This is useful for tracking and managing individual completions or retrieving the results of a particular request.
    /// </remarks>
    property Id: string read FId write FId;

    /// <summary>
    /// The object type, which is always "codestral.completion".
    /// </summary>
    /// <remarks>
    /// The <c>Object</c> property describes the type of the response. For codestral completions, this value is always "codestral.completion",
    /// providing a clear indication of the response type when working with multiple object types in a system.
    /// </remarks>
    property &Object: string read FObject write FObject;

    /// <summary>
    /// The Unix timestamp (in seconds) of when the codestral completion was created.
    /// </summary>
    /// <remarks>
    /// The <c>Created</c> property contains a Unix timestamp indicating when the AI generated the codestral completion.
    /// This is useful for logging, auditing, or ordering completions chronologically.
    /// </remarks>
    property Created: Int64 read FCreated write FCreated;

    /// <summary>
    /// The model used for the codestral completion.
    /// </summary>
    /// <remarks>
    /// The <c>Model</c> property specifies which AI model was used to generate the codestral completion. This can be helpful
    /// when comparing results across different models or tracking which model versions are producing responses.
    /// </remarks>
    property Model: string read FModel write FModel;

    /// <summary>
    /// A list of codestral completion choices generated by the model.
    /// </summary>
    /// <remarks>
    /// The <c>Choices</c> property holds an array of <c>TCodestralChoices</c> objects, representing different response options
    /// generated by the AI model. There may be multiple choices if the request specified more than one completion.
    /// </remarks>
    property Choices: TArray<TCodestralChoices> read FChoices write FChoices;

    /// <summary>
    /// Usage statistics for the completion request, including token counts for the prompt and completion.
    /// </summary>
    /// <remarks>
    /// The <c>Usage</c> property provides information about the number of tokens consumed during the request, including those
    /// used in the prompt and those generated in the completion. This data is important for monitoring API usage and costs.
    /// </remarks>
    property Usage: TCodestralUsage read FUsage write FUsage;

    /// <summary>
    /// Destructor to clean up resources used by this <c>TCodestral</c> instance.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that any allocated resources, such as the memory for the array of choices or usage statistics, are
    /// properly released when the object is no longer needed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Manages asynchronous chat events for a chat request using <c>TCodestral</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynCodeParams</c> type extends the <c>TAsynParams&lt;TCodestral&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynCodeParams = TAsyncCallBack<TCodestral>;

  /// <summary>
  /// Manages asynchronous streaming chat events for a chat request using <c>TCodestral</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynCodeStreamParams</c> type extends the <c>TAsynStreamParams&lt;TCodestral&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynCodeStreamParams = TAsyncStreamCallBack<TCodestral>;

  /// <summary>
  /// Represents a callback procedure used during the reception of responses from a codestral request in streaming mode.
  /// </summary>
  /// <param name="Codestral">
  /// The <c>TCodestral</c> object containing the current information about the response generated by the model.
  /// If this value is <c>nil</c>, it indicates that the data stream is complete.
  /// </param>
  /// <param name="IsDone">
  /// A boolean flag indicating whether the streaming process is complete.
  /// If <c>True</c>, it means the model has finished sending all response data.
  /// </param>
  /// <param name="Cancel">
  /// A boolean flag that can be set to <c>True</c> within the callback to cancel the streaming process.
  /// If set to <c>True</c>, the streaming will be terminated immediately.
  /// </param>
  /// <remarks>
  /// This callback is invoked multiple times during the reception of the response data from the model.
  /// It allows for real-time processing of received messages and interaction with the user interface or other systems
  /// based on the state of the data stream.
  /// When the <c>IsDone</c> parameter is <c>True</c>, it indicates that the model has finished responding,
  /// and the <c>Codestral</c> parameter will be <c>nil</c>.
  /// </remarks>
  TCodestralEvent = reference to procedure(var Codestral: TCodestral; IsDone: Boolean; var Cancel: Boolean);

  /// <summary>
  /// The <c>TCodestralRoute</c> class inherits from <c>TMistralAIAPIRoute</c> and provides an interface for managing various interactions with the codestral API.
  /// It supports creating codestral completion requests in synchronous, asynchronous, and streaming modes, offering mechanisms to handle responses generated by the model.
  /// </summary>
  /// <remarks>
  /// This class facilitates sending messages to a codestral model, receiving responses, and managing them, whether synchronously or asynchronously.
  /// The primary methods in the class are:
  /// <list type="bullet">
  /// <item>
  /// <term><c>Create</c></term>
  /// <description>Sends a codestral request and waits for a full response.</description>
  /// </item>
  /// <item>
  /// <term><c>AsyncCreate</c></term>
  /// <description>Performs an asynchronous codestral completion request with event handling.</description>
  /// </item>
  /// <item>
  /// <term><c>CreateStream</c></term>
  /// <description>Initiates a codestral completion request in streaming mode, receiving tokens progressively.</description>
  /// </item>
  /// <item>
  /// <term><c>ASyncCreateStream</c></term>
  /// <description>Performs an asynchronous request in streaming mode with event handling.</description>
  /// </item>
  /// </list>
  /// Each method allows configuring model parameters, setting input messages, managing token limits, and including callbacks for processing responses or errors.
  /// </remarks>
  TCodestralRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Creates an asynchronous completion for codestral message.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the codestral request, such as model selection, messages, and other parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a record containing event handlers for the asynchronous codestral completion, such as on success and on error.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous request to generate a codestral completion based on the provided parameters. The response or error is handled by the provided callBacks.
    /// <code>
    /// MistralAI.Codestral.AsyncCreate(
    ///   procedure (Params: TCodestralParams)
    ///   begin
    ///     Params.Model('my_model');
    ///     Params.Prompt('Generate a code snippet');
    ///     Params.MaxTokens(1024);
    ///   end,
    ///   function: TAsynCodeParams
    ///   begin
    ///     Result.Sender := Memo1;  // callBacks will return this instance
    ///
    ///     Result.OnStart := nil;   // if nil then; Can be omitted
    ///
    ///     Result.OnSuccess := procedure (Sender: TObject; Code: TCodeStral)
    ///     begin
    ///       var M := Sender as TMemo;
    ///       for var Choice in Code.Choices do
    ///         M.Lines.Add(Choice.Message.Content + sLineBreak);
    ///     end;
    ///
    ///     Result.OnError := procedure (Sender: TObject; Value: string)
    ///     begin
    ///       ShowMessage(Value);
    ///     end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncCreate(ParamProc: TProc<TCodestralParams>; CallBacks: TFunc<TAsynCodeParams>);
    /// <summary>
    /// Creates an asynchronous streaming codestral completion request.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the codestral request, including the model, messages, and additional options such as max tokens and streaming mode.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynCodeStreamParams</c> record which contains event handlers for managing different stages of the streaming process: progress updates, success, errors, and cancellation.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous codestral operation in streaming mode, where tokens are progressively received and processed.
    /// The provided event handlers allow for handling progress (i.e., receiving tokens in real time), detecting success, managing errors, and enabling cancellation logic.
    /// <code>
    /// MistralAI.Codestral.AsyncCreateStream(
    ///   procedure(Params: TCodestralParams)
    ///   begin
    ///     Params.Model('my_model');
    ///     Params.Prompt('Generate a code snippet');
    ///     Params.MaxTokens(1024);
    ///     Params.Stream;
    ///   end,
    ///
    ///   function: TAsynCodeStreamParams
    ///   begin
    ///     Result.Sender := Memo1;
    ///
    ///     Result.OnProgress :=
    ///       procedure (Sender: TObject; Codestral: TCodestral)
    ///       begin
    ///         // Handle progressive updates to the codestral response
    ///       end;
    ///
    ///     Result.OnSuccess :=
    ///       procedure (Sender: TObject)
    ///       begin
    ///         // Handle success when the operation completes
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure (Sender: TObject; Value: string)
    ///       begin
    ///         ShowMessage(Value); // Display error message
    ///       end;
    ///
    ///     Result.OnDoCancel :=
    ///       function: Boolean
    ///       begin
    ///         Result := CheckBox1.Checked; // Click on checkbox to cancel
    ///       end;
    ///
    ///     Result.OnCancellation :=
    ///       procedure (Sender: TObject)
    ///       begin
    ///         // Processing when process has been canceled
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncCreateStream(ParamProc: TProc<TCodestralParams>;
      CallBacks: TFunc<TAsynCodeStreamParams>);
    /// <summary>
    /// Creates a completion for the codestral message using the provided parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the codestral request, such as selecting the model, providing messages, setting token limits, etc.
    /// </param>
    /// <returns>
    /// Returns a <c>TCodestral</c> object that contains the codestral response, including the choices generated by the model.
    /// </returns>
    /// <exception cref="MistralAIExceptionAPI">
    /// Thrown when there is an error in the communication with the API or other underlying issues in the API call.
    /// </exception>
    /// <exception cref="MistralAIExceptionInvalidRequestError">
    /// Thrown when the request is invalid, such as when required parameters are missing or values exceed allowed limits.
    /// </exception>
    /// <remarks>
    /// The <c>Create</c> method sends a codestral completion request and waits for the full response. The returned <c>TCodestral</c> object contains the model's generated response, including multiple choices if available.
    /// Example usage:
    /// <code>
    /// var
    ///   Codestral: TCodestral;
    /// begin
    ///   Codestral := MistralAI.Codestral.Create(
    ///     procedure (Params: TCodestralParams)
    ///     begin
    ///       Params.Model('my_model');
    ///       Params.Prompt('Generate a code snippet');
    ///       Params.MaxTokens(1024);
    ///     end);
    ///   try
    ///     for var Choice in Codestral.Choices do
    ///       Memo1.Lines.Add(Choice.Message.Content);
    ///   finally
    ///     Codestral.Free;
    ///   end;
    /// end;
    /// </code>
    /// </remarks>
    function Create(ParamProc: TProc<TCodestralParams>): TCodestral;
    /// <summary>
    /// Creates a codestral message completion with a streamed response.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the codestral request, such as selecting the model, providing messages, and adjusting other settings like token limits or temperature.
    /// </param>
    /// <param name="Event">
    /// A callback of type <c>TCodestralEvent</c> that is triggered with each chunk of data received during the streaming process. It includes the current state of the <c>TCodestral</c> object, a flag indicating if the stream is done, and a boolean to handle cancellation.
    /// </param>
    /// <returns>
    /// Returns <c>True</c> if the streaming process started successfully, <c>False</c> otherwise.
    /// </returns>
    /// <remarks>
    /// This method initiates a codestral request in streaming mode, where the response is delivered incrementally in real-time.
    /// The <c>Event</c> callback will be invoked multiple times as tokens are received.
    /// When the response is complete, the <c>IsDone</c> flag will be set to <c>True</c>, and the <c>Codestral</c> object will be <c>nil</c>.
    /// The streaming process can be interrupted by setting the <c>Cancel</c> flag to <c>True</c> within the event.
    /// Example usage:
    /// <code>
    /// var
    ///   Codestral: TCodestral;
    /// begin
    ///   MistralAI.Codestral.CreateStream(
    ///     procedure (Params: TCodestralParams)
    ///     begin
    ///       Params.Model('my_model');
    ///       Params.Prompt('Generate a code snippet');
    ///       Params.MaxTokens(1024);
    ///       Params.Stream;
    ///     end,
    ///     procedure(var Codestral: TCodestral; IsDone: Boolean; var Cancel: Boolean)
    ///     begin
    ///       if IsDone then
    ///         Memo1.Lines.Add('Stream completed')
    ///       else if Assigned(Codestral) then
    ///         Memo1.Lines.Add(Codestral.Choices[0].Message.Content);
    ///
    ///       // Cancel streaming if needed
    ///       Cancel := CheckBox1.Checked;
    ///     end
    ///   );
    /// end;
    /// </code>
    /// </remarks>
    function CreateStream(ParamProc: TProc<TCodestralParams>; Event: TCodestralEvent): Boolean;
  end;

implementation

uses
  System.StrUtils, Rest.Json, System.Rtti;

{ TCodestralParams }

constructor TCodestralParams.Create;
begin
  inherited;
  Model('codestral-latest');
end;

function TCodestralParams.MaxTokens(const Value: Integer): TCodestralParams;
begin
  Result := TCodestralParams(Add('max_tokens', Value));
end;

function TCodestralParams.MinTokens(const Value: Integer): TCodestralParams;
begin
  Result := TCodestralParams(Add('min_tokens', Value));
end;

function TCodestralParams.Model(const Value: string): TCodestralParams;
begin
  Result := TCodestralParams(Add('model', Value));
end;

function TCodestralParams.Prompt(const Value: string): TCodestralParams;
begin
  Result := TCodestralParams(Add('prompt', Value));
end;

function TCodestralParams.RandomSeed(const Value: Integer): TCodestralParams;
begin
  Result := TCodestralParams(Add('random_seed', Value));
end;

function TCodestralParams.Stop(const Value: TArray<string>): TCodestralParams;
var
  Items: TJSONArray;
begin
  Items := TJSONArray.Create;
  try
    for var Item in Value do
      begin
        Items.Add(Item);
      end;
  except
    Items.Free;
    raise;
  end;
  Result := TCodestralParams(Add('stop', Items));
end;

function TCodestralParams.Stream(const Value: Boolean): TCodestralParams;
begin
  Result := TCodestralParams(Add('stream', Value));
end;

function TCodestralParams.Suffix(const Value: string): TCodestralParams;
begin
  Result := TCodestralParams(Add('suffix', Value));
end;

function TCodestralParams.Temperature(const Value: Single): TCodestralParams;
begin
  Result := TCodestralParams(Add('temperature', Value));
end;

function TCodestralParams.TopP(const Value: Single): TCodestralParams;
begin
  Result := TCodestralParams(Add('top_p', Value));
end;

{ TCodestralFinishReasonHelper }

class function TCodestralFinishReasonHelper.Create(
  const Value: string): TCodestralFinishReason;
begin
  case IndexStr(AnsiLowerCase(Value), ['stop', 'length', 'model_length', 'error']) of
    0 :
      Exit(stop);
    1 :
      Exit(length_limite);
    2 :
      Exit(model_length);
    3 :
      Exit(error);
  end;
  Result := stop;
end;

function TCodestralFinishReasonHelper.ToString: string;
begin
  case Self of
    stop:
      Exit('stop');
    length_limite:
      Exit('length');
    model_length:
      Exit('model_length');
    error:
      Exit('error');
  end;
end;

{ TCodestralFinishReasonInterceptor }

function TCodestralFinishReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TCodestralFinishReason>.ToString;
end;

procedure TCodestralFinishReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TCodestralFinishReason.Create(Arg)));
end;

{ TCodestralChoices }

destructor TCodestralChoices.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FDelta) then
    FDelta.Free;
  inherited;
end;

{ TCodestral }

destructor TCodestral.Destroy;
begin
  if Assigned(FUsage) then
    FUsage.Free;
  for var Item in FChoices do
    Item.Free;
  inherited;
end;

{ TCodestralRoute }

procedure TCodestralRoute.AsyncCreate(ParamProc: TProc<TCodestralParams>;
  CallBacks: TFunc<TAsynCodeParams>);
begin
  with TAsyncCallBackExec<TAsynCodeParams, TCodestral>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TCodestral
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TCodestralRoute.AsyncCreateStream(ParamProc: TProc<TCodestralParams>;
  CallBacks: TFunc<TAsynCodeStreamParams>);
begin
  var CallBackParams := TUseParamsFactory<TAsynCodeStreamParams>.CreateInstance(CallBacks);

  var Sender := CallBackParams.Param.Sender;
  var OnStart := CallBackParams.Param.OnStart;
  var OnSuccess := CallBackParams.Param.OnSuccess;
  var OnProgress := CallBackParams.Param.OnProgress;
  var OnError := CallBackParams.Param.OnError;
  var OnCancellation := CallBackParams.Param.OnCancellation;
  var OnDoCancel := CallBackParams.Param.OnDoCancel;

  var Task: ITask := TTask.Create(
          procedure()
          begin
            {--- Pass the instance of the current class in case no value was specified. }
            if not Assigned(Sender) then
              Sender := Self;

            {--- Trigger OnStart callback }
            if Assigned(OnStart) then
              TThread.Queue(nil,
                procedure
                begin
                  OnStart(Sender);
                end);
            try
              var Stop := False;

              {--- Processing }
              CreateStream(ParamProc,
                procedure (var Code: TCodestral; IsDone: Boolean; var Cancel: Boolean)
                begin
                  {--- Check that the process has not been canceled }
                  if Assigned(OnDoCancel) then
                    TThread.Queue(nil,
                        procedure
                        begin
                          Stop := OnDoCancel();
                        end);
                  if Stop then
                    begin
                      {--- Trigger when processus was stopped }
                      if Assigned(OnCancellation) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnCancellation(Sender)
                        end);
                      Cancel := True;
                      Exit;
                    end;
                  if not IsDone and Assigned(Code) then
                    begin
                      var LocalCode := Code;
                      Code := nil;

                      {--- Triggered when processus is progressing }
                      if Assigned(OnProgress) then
                        TThread.Synchronize(TThread.Current,
                        procedure
                        begin
                          try
                            OnProgress(Sender, LocalCode);
                          finally
                            {--- Makes sure to release the instance containing the data obtained
                                 following processing}
                            LocalCode.Free;
                          end;
                        end);
                    end
                  else
                  if IsDone then
                    begin
                      {--- Trigger OnEnd callback when the process is done }
                      if Assigned(OnSuccess) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnSuccess(Sender);
                        end);
                    end;
                end);
            except
              on E: Exception do
                begin
                  var Error := AcquireExceptionObject;
                  try
                    var ErrorMsg := (Error as Exception).Message;

                    {--- Trigger OnError callback if the process has failed }
                    if Assigned(OnError) then
                      TThread.Queue(nil,
                      procedure
                      begin
                        OnError(Sender, ErrorMsg);
                      end);
                  finally
                    {--- Ensures that the instance of the caught exception is released}
                    Error.Free;
                  end;
                end;
            end;
          end);
  Task.Start;
end;

function TCodestralRoute.Create(
  ParamProc: TProc<TCodestralParams>): TCodestral;
begin
  Result := API.Post<TCodestral, TCodestralParams>('fim/completions', ParamProc);
end;

function TCodestralRoute.CreateStream(ParamProc: TProc<TCodestralParams>;
  Event: TCodestralEvent): Boolean;
var
  Response: TStringStream;
  RetPos: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    RetPos := 0;
    Result := API.Post<TCodestralParams>('fim/completions', ParamProc, Response,
      procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
      var
        IsDone: Boolean;
        Data: string;
        Code: TCodestral;
        TextBuffer: string;
        Line: string;
        Ret: Integer;
      begin
        try
          TextBuffer := Response.DataString;
        except
          on E: EEncodingError do
            Exit;
        end;

        repeat
          Ret := TextBuffer.IndexOf(#10, RetPos);
          if Ret < 0 then
            Continue;
          Line := TextBuffer.Substring(RetPos, Ret - RetPos);
          RetPos := Ret + 1;

          if Line.IsEmpty or Line.StartsWith(#10) then
            Continue;
          Code := nil;
          Data := Line.Replace('data: ', '').Trim([' ', #13, #10]);
          IsDone := Data = '[DONE]';

          if not IsDone then
          try
            Code := TJson.JsonToObject<TCodestral>(Data);
          except
            Code := nil;
          end;

          try
            Event(Code, IsDone, AAbort);
          finally
            Code.Free;
          end;
        until Ret < 0;

      end);
  finally
    Response.Free;
  end;
end;

end.
