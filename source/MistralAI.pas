unit MistralAI;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, MistralAI.API, System.Net.URLClient,
  MistralAI.Chat, MistralAI.Embeddings, MistralAI.Models, MistralAI.Codestral,
  MistralAI.Files, MistralAI.FineTunings, MistralAI.Agents, MistralAI.Classifiers,
  MistralAI.Functions.Tools, MistralAI.Functions.Core, MistralAI.Batch;

type
  /// <summary>
  /// The IMistralAI interface provides access to the various features and routes of the Mistral AI API.
  /// This interface allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This interface should be implemented by any class that wants to provide a structured way of accessing
  /// the Mistral AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  ///
  /// To use this interface, instantiate a class that implements it, set the required properties such as
  /// <see cref="Token"/> and <see cref="BaseURL"/>, and call the relevant methods for the desired operations.
  /// <code>
  ///   var MistralAI: IMistralAI := TMistralAI.Create(API_TOKEN);
  ///   or
  ///   var CodestralAI: IMistralAI := TMistralAI.Create(API_TOKEN, [CodestralSpec]);
  /// </code>
  /// <seealso cref="TMistralAI"/>
  /// </remarks>
  IMistralAI = interface
    ['{CB506753-77B2-4BD6-A2F8-216433D444A8}']
    function GetAPI: TMistralAIAPI;
    procedure SetToken(const Value: string);
    function GetToken: string;
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetAgentRoute: TAgentRoute;
    function GetChatRoute: TChatRoute;
    function GetCodestralRoute: TCodestralRoute;
    function GetEmbeddingsRoute: TEmbeddingsRoute;
    function GetFilesRoute: TFilesRoute;
    function GetFineTuningRoute: TFineTuningRoute;
    function GetModelsRoute: TModelsRoute;
    function GetClassifiersRoute: TClassifiersRoute;
    function GetBatchRoute: TBatchRoute;

    property Agent: TAgentRoute read GetAgentRoute;
    /// <summary>
    /// A batch is composed of a list of API requests. The structure of an individual request includes:
    /// <para>
    /// - A unique custom_id for identifying each request and referencing results after completion
    /// </para>
    /// <para>
    /// - A body object with message information
    /// </para>
    /// </summary>
    /// <returns>
    /// An instance of GetBatchRoute for batch-related operations.
    /// </returns>
    property Batch: TBatchRoute read GetBatchRoute;
    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the Codestral Completion API.
    /// Allows for code design or completion using a template configured to follow specific instructions.
    /// </summary>
    /// <returns>
    /// An instance of TCodestralRoute for code completion operations.
    /// </returns>
    property Codestral: TCodestralRoute read GetCodestralRoute;
    /// <summary>
    /// Provides access to the embeddings API.
    /// Enables the embedding of sentences or documents.
    /// </summary>
    /// <returns>
    /// An instance of TEmbeddingsRoute for embedding operations.
    /// </returns>
    property Embeddings: TEmbeddingsRoute read GetEmbeddingsRoute;
    /// <summary>
    /// Provides access to the file management API.
    /// Files can be uploaded and used with features such as fine-tuning.
    /// </summary>
    /// <returns>
    /// An instance of TFilesRoute for file-related operations.
    /// </returns>
    property &File: TFilesRoute read GetFilesRoute;
    /// <summary>
    /// Provides access to fine-tuning API for user and organization.
    /// Allows managing fine-tuning jobs.
    /// </summary>
    /// <returns>
    /// An instance of TFineTuningRoute for fine-tuning operations.
    /// </returns>
    property FineTuning: TFineTuningRoute read GetFineTuningRoute;
    /// <summary>
    /// Lists and describes the various models available in the API.
    /// You can refer to the Models documentation to understand what models are available and the differences between them.
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for model-related operations.
    /// </returns>
    property Models: TModelsRoute read GetModelsRoute;
    /// <summary>
    /// Moderation service, which is powered by the Mistral Moderation model, Ministral 8B 24.10
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for model-related operations.
    /// </returns>
    property Classifiers: TClassifiersRoute read GetClassifiersRoute;
    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TMistralAIAPI for making API calls.
    /// </returns>
    property API: TMistralAIAPI read GetAPI;
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.mistral.ai/v1.
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;
    /// <summary>
    /// Provides access to agent completion API.
    /// An AI agent is an autonomous system using large language models (LLM) to perform tasks based on high-level instructions.
    /// </summary>
    /// <returns>
    /// An instance of TAgentRoute for agent-related operations.
    /// </returns>
  end;

  /// <summary>
  /// Specification taken into account
  /// </summary>
  TSpec = (
    /// <summary>
    /// The "codestral" specification is taken into account in the instantiation of the class
    /// </summary>
    CodestralSpec);

  /// <summary>
  /// List of specifications taken into account
  /// </summary>
  TSpecs = set of TSpec;

  TMistralAIFactory = class
    class function CreateInstance(const AToken: string; Specs: TSpecs = []): IMistralAI;
  end;

  /// <summary>
  /// The TMistralAI class provides access to the various features and routes of the Mistral AI API.
  /// This class allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This class should be implemented by any class that wants to provide a structured way of accessing
  /// the Mistral AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  /// <seealso cref="TMistralAI"/>
  /// </remarks>
  TMistralAI = class(TInterfacedObject, IMistralAI)
  strict private
    FSpecs: TSpecs;
    procedure CodestralCheck;

  private
    FAPI: TMistralAIAPI;
    FAgentRoute: TAgentRoute;
    FChatRoute: TChatRoute;
    FCodestralRoute: TCodestralRoute;
    FEmbeddingsRoute: TEmbeddingsRoute;
    FFileRoute: TFilesRoute;
    FFineTuningRoute: TFineTuningRoute;
    FModelsRoute: TModelsRoute;
    FClassifiersRoute: TClassifiersRoute;
    FBatchRoute: TBatchRoute;
    function GetAPI: TMistralAIAPI;
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetAgentRoute: TAgentRoute;
    function GetChatRoute: TChatRoute;
    function GetCodestralRoute: TCodestralRoute;
    function GetEmbeddingsRoute: TEmbeddingsRoute;
    function GetFilesRoute: TFilesRoute;
    function GetFineTuningRoute: TFineTuningRoute;
    function GetModelsRoute: TModelsRoute;
    function GetClassifiersRoute: TClassifiersRoute;
    function GetBatchRoute: TBatchRoute;
  public
    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TMistralAIAPI for making API calls.
    /// </returns>
    property API: TMistralAIAPI read GetAPI;
    /// <summary>
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.mistral.ai/v1.
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;

  public
    /// <summary>
    /// Provides access to agent completion API.
    /// An AI agent is an autonomous system using large language models (LLM) to perform tasks based on high-level instructions.
    /// </summary>
    /// <returns>
    /// An instance of TAgentRoute for agent-related operations.
    /// </returns>
    property Agent: TAgentRoute read GetAgentRoute;
    /// <summary>
    /// A batch is composed of a list of API requests. The structure of an individual request includes:
    /// <para>
    /// - A unique custom_id for identifying each request and referencing results after completion
    /// </para>
    /// <para>
    /// - A body object with message information
    /// </para>
    /// </summary>
    /// <returns>
    /// An instance of GetBatchRoute for batch-related operations.
    /// </returns>
    property Batch: TBatchRoute read GetBatchRoute;
    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the Codestral Completion API.
    /// Allows for code design or completion using a template configured to follow specific instructions.
    /// </summary>
    /// <returns>
    /// An instance of TCodestralRoute for code completion operations.
    /// </returns>
    property Codestral: TCodestralRoute read GetCodestralRoute;
    /// <summary>
    /// Provides access to the embeddings API.
    /// Enables the embedding of sentences or documents.
    /// </summary>
    /// <returns>
    /// An instance of TEmbeddingsRoute for embedding operations.
    /// </returns>
    property Embeddings: TEmbeddingsRoute read GetEmbeddingsRoute;
    /// <summary>
    /// Provides access to the file management API.
    /// Files can be uploaded and used with features such as fine-tuning.
    /// </summary>
    /// <returns>
    /// An instance of TFilesRoute for file-related operations.
    /// </returns>
    property &File: TFilesRoute read GetFilesRoute;
    /// <summary>
    /// Provides access to fine-tuning API for user and organization.
    /// Allows managing fine-tuning jobs.
    /// </summary>
    /// <returns>
    /// An instance of TFineTuningRoute for fine-tuning operations.
    /// </returns>
    property FineTuning: TFineTuningRoute read GetFineTuningRoute;
    /// <summary>
    /// Lists and describes the various models available in the API.
    /// You can refer to the Models documentation to understand what models are available and the differences between them.
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for model-related operations.
    /// </returns>
    property Models: TModelsRoute read GetModelsRoute;
    /// <summary>
    /// Moderation service, which is powered by the Mistral Moderation model, Ministral 8B 24.10
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for model-related operations.
    /// </returns>
    property Classifiers: TClassifiersRoute read GetClassifiersRoute;
  public
    constructor Create; overload;
    constructor Create(const AToken: string; Specs: TSpecs = []); overload;
    destructor Destroy; override;
  end;

  {$REGION 'MistralAI.Chat'}

  /// <summary>
  /// Represents a message payload that includes an image, identified by a URL or a base-64 encoded string.
  /// </summary>
  /// <remarks>
  /// This class is used to encapsulate details of an image message, including its source (URL or base-64 string)
  /// and additional descriptive information. It extends <c>TJSONParam</c> for JSON serialization and integration with APIs.
  /// </remarks>
  /// <example>
  /// This class can be utilized to construct structured image messages for communication in chat systems.
  /// </example>
  TMessageImageURL = MistralAI.Chat.TMessageImageURL;

  /// <summary>
  /// Represents the content of a message, which can be either text or an image URL.
  /// </summary>
  /// <remarks>
  /// This class is used to define the structure and type of content within a message,
  /// such as textual data or image references. It extends <c>TJSONParam</c> for
  /// seamless JSON serialization and integration with APIs.
  /// </remarks>
  TMessageContent = MistralAI.Chat.TMessageContent;

  /// <summary>
  /// Represents the metadata and arguments of a function invoked during a chat interaction.
  /// </summary>
  /// <remarks>
  /// This class is used to define the structure of function calls, including the function's name and its associated arguments.
  /// It is designed to be serialized into JSON format for integration with APIs or other systems requiring structured function data.
  /// </remarks>
  TFunctionCalled = MistralAI.Chat.TFunctionCalled;

  /// <summary>
  /// Represents a tool call configuration used in chat-based operations.
  /// </summary>
  /// <remarks>
  /// This class is designed to define and manage tool calls within chat contexts, including
  /// their type, identifier, and associated functions. It integrates with <c>TJSONParam</c>
  /// for seamless serialization and API interactions.
  /// </remarks>
  TToolCalls = MistralAI.Chat.TToolCalls;

  /// <summary>
  /// Represents the payload structure for a chat message in the context of a conversation.
  /// </summary>
  /// <remarks>
  /// This class encapsulates the essential elements of a chat message, including its role, content,
  /// and associated metadata. It extends <c>TJSONParam</c>, enabling seamless integration with JSON-based
  /// APIs for chat functionalities. This class is a foundational building block for managing the flow of
  /// conversations between users, systems, and assistants.
  /// </remarks>
  TChatMessagePayload = MistralAI.Chat.TChatMessagePayload;

  /// <summary>
  /// Represents the payload structure for a chat message in the context of a conversation.
  /// </summary>
  /// <remarks>
  /// This class encapsulates the essential elements of a chat message, including its role, content,
  /// and associated metadata. It extends <c>TJSONParam</c>, enabling seamless integration with JSON-based
  /// APIs for chat functionalities. This class is a foundational building block for managing the flow of
  /// conversations between users, systems, and assistants.
  /// </remarks>
  PayLoad = MistralAI.Chat.PayLoad;

  /// <summary>
  /// The <c>TChatParams</c> class represents the set of parameters used to configure a chat interaction with an AI model.
  /// </summary>
  /// <remarks>
  /// This class allows you to define various settings that control how the model behaves, including which model to use, how many tokens to generate,
  /// what kind of messages to send, and how the model should handle its output. By using this class, you can fine-tune the AI's behavior and response format
  /// based on your application's specific needs.
  /// <para>
  /// It inherits from <c>TJSONParam</c>, which provides methods for handling and serializing the parameters as JSON, allowing seamless integration
  /// with JSON-based APIs.
  /// </para>
  /// <code>
  /// var
  ///   Params: TChatParams;
  /// begin
  ///   Params := TChatParams.Create
  ///     .Model('my_model')
  ///     .MaxTokens(100)
  ///     .Messages([TChatMessagePayload.User('Hello!')])
  ///     .ResponseFormat('json_object')
  ///     .Temperature(0.7)
  ///     .TopP(1)
  ///     .SafePrompt(True);
  /// end;
  /// </code>
  /// This example shows how to instantiate and configure a <c>TChatParams</c> object for interacting with an AI model.
  /// </remarks>
  TChatParams = MistralAI.Chat.TChatParams;

  /// <summary>
  /// Represents the token usage statistics for a chat interaction, including the number of tokens
  /// used in the prompt, the completion, and the total number of tokens consumed.
  /// </summary>
  /// <remarks>
  /// The <c>TChatUsage</c> class provides insight into the number of tokens used during a chat interaction.
  /// This information is critical for understanding the cost of a request when using token-based billing systems
  /// or for monitoring the model's behavior in terms of input (prompt) and output (completion) size.
  /// </remarks>
  TChatUsage = MistralAI.Chat.TChatUsage;

  /// <summary>
  /// Represents a chat message exchanged between participants (user, assistant, or system) in a conversation.
  /// </summary>
  /// <remarks>
  /// The <c>TChatMessage</c> class encapsulates the essential information of a message within a chat application, including:
  /// - The role of the sender (user, assistant, or system).
  /// - The content of the message itself.
  /// - Optionally, a list of tool calls that may be required to complete the message response.
  /// This class is fundamental for managing the flow of a conversation, allowing the system to track who said what and what actions need to be taken.
  /// </remarks>
  TChatMessage = MistralAI.Chat.TChatMessage;

  /// <summary>
  /// Represents a single completion option generated by the AI model during a chat interaction.
  /// </summary>
  /// <remarks>
  /// The <c>TChatChoices</c> class stores the results of the AI model's response to a user prompt. Each instance of this class represents one of potentially
  /// many choices that the model could return. This includes:
  /// - An index identifying the choice.
  /// - A message generated by the model.
  /// - Optional deltas for streamed responses.
  /// - The reason the model stopped generating tokens.
  /// This class is useful when multiple potential responses are generated and evaluated, or when streaming responses incrementally.
  /// </remarks>
  TChatChoices = MistralAI.Chat.TChatChoices;

  /// <summary>
  /// Represents a chat completion response generated by an AI model, containing the necessary metadata,
  /// the generated choices, and usage statistics.
  /// </summary>
  /// <remarks>
  /// The <c>TChat</c> class encapsulates the results of a chat request made to an AI model.
  /// It contains details such as a unique identifier, the model used, when the completion was created,
  /// the choices generated by the model, and token usage statistics.
  /// This class is crucial for managing the results of AI-driven conversations and understanding the
  /// underlying usage and response characteristics of the AI.
  /// </remarks>
  TChat = MistralAI.Chat.TChat;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChat</c> type extends the <c>TAsynParams&lt;TChat&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynChat = MistralAI.Chat.TAsynChat;

  /// <summary>
  /// Manages asynchronous streaming chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChatStream</c> type extends the <c>TAsynStreamParams&lt;TChat&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynChatStream = MistralAI.Chat.TAsynChatStream;

  /// <summary>
  /// Represents a callback procedure used during the reception of responses from a chat request in streaming mode.
  /// </summary>
  /// <param name="Chat">
  /// The <c>TChat</c> object containing the current information about the response generated by the model.
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
  /// and the <c>Chat</c> parameter will be <c>nil</c>.
  /// </remarks>
  TChatEvent = MistralAI.Chat.TChatEvent;

  {$ENDREGION}

  {$REGION 'MistralAI.Embeddings'}

  /// <summary>
  /// Represents the parameters for an embedding request.
  /// </summary>
  /// <remarks>
  /// Use this class to specify the input text and other parameters for generating embeddings.
  /// </remarks>
  TEmbeddingParams = MistralAI.Embeddings.TEmbeddingParams;

  /// <summary>
  /// Represents token usage statistics for an embedding request.
  /// </summary>
  /// <remarks>
  /// Contains information about the number of tokens used in the request.
  /// </remarks>
  TEmbeddingUsage = MistralAI.Embeddings.TEmbeddingUsage;

  /// <summary>
  /// Represents an embedding result for a single input.
  /// </summary>
  /// <remarks>
  /// Contains the embedding vector and associated metadata for a single input text.
  /// </remarks>
  TEmbeddingData = MistralAI.Embeddings.TEmbeddingData;

  /// <summary>
  /// Represents the response from an embedding request.
  /// </summary>
  /// <remarks>
  /// Contains the embeddings, model information, and usage statistics returned by the API.
  /// </remarks>
  TEmbeddings = MistralAI.Embeddings.TEmbeddings;

  /// <summary>
  /// Represents the asynchronous callback parameters for embedding requests.
  /// </summary>
  /// <remarks>
  /// Used to handle asynchronous responses for embedding operations.
  /// </remarks>
  TAsyncEmbeddings = MistralAI.Embeddings.TAsyncEmbeddings;

  {$ENDREGION}

  {$REGION 'MistralAI.Models'}

  /// <summary>
  /// The TModelParams class is used to specify parameters for updating the properties
  /// of a fine-tuned model in the MistralAI environment. It allows you to set new values
  /// for the model's name and description.
  /// </summary>
  /// <remarks>
  /// This class provides a fluent interface for configuring model parameters before
  /// submitting an update request. The Name and Description methods allow you to set
  /// these attributes individually and return the updated TModelParams object, enabling
  /// method chaining for concise configuration.
  /// </remarks>
  TModelParams = MistralAI.Models.TModelParams;

  /// <summary>
  /// The TCapabilities class represents the various features and functionalities
  /// that a MistralAI model can support. It indicates whether the model is capable of
  /// performing specific tasks such as chat completion, function calling, or fine-tuning.
  /// </summary>
  /// <remarks>
  /// This class is used to describe the capabilities of a model, helping users
  /// to understand what operations can be performed with the model. Each property
  /// corresponds to a specific capability, which can be checked to see if the model
  /// supports that feature. This information is typically used when selecting or
  /// configuring models for specific use cases.
  /// </remarks>
  TCapabilities = MistralAI.Models.TCapabilities;

  /// <summary>
  /// The TCoreModel class represents a generic model object that can be used with the MistralAI API.
  /// It contains all the essential properties and metadata required to describe a model, including
  /// its identifier, creation date, capabilities, and other attributes.
  /// </summary>
  /// <remarks>
  /// This class serves as the base class for more specific model types and provides a comprehensive
  /// view of a model's characteristics. The properties in this class are primarily used for
  /// interacting with the MistralAI API and retrieving model metadata. It is designed to be
  /// extended by other classes that represent more specialized model types.
  /// </remarks>
  TCoreModel = MistralAI.Models.TCoreModel;

  /// <summary>
  /// The TModel class extends TCoreModel to include additional information about
  /// the model's deprecation status. It inherits all the properties of TCoreModel
  /// and adds a field to track when the model is considered deprecated.
  /// </summary>
  /// <remarks>
  /// This class is used to represent models that are still available in the MistralAI environment
  /// but are marked as deprecated. Deprecation indicates that the model may no longer be supported
  /// in future versions, and users are encouraged to migrate to newer models if possible.
  /// This class is particularly useful for managing model lifecycles and ensuring that applications
  /// do not rely on outdated or unsupported models.
  /// </remarks>
  TModel = MistralAI.Models.TModel;

  /// <summary>
  /// The TFineTunedModel class extends the TCoreModel class to represent a fine-tuned model
  /// within the MistralAI environment. It includes additional information specific to the
  /// fine-tuning process, such as the job identifier associated with the fine-tuning operation.
  /// </summary>
  /// <remarks>
  /// This class is used to manage and interact with fine-tuned models, which are variants of
  /// base models that have been further trained on specific datasets to improve performance
  /// in specialized tasks. It inherits all properties from TCoreModel and adds specific
  /// attributes related to the fine-tuning process.
  /// </remarks>
  TFineTunedModel = MistralAI.Models.TFineTunedModel;

  /// <summary>
  /// The TModels class represents a collection of available models in the MistralAI environment.
  /// It provides basic information about each model, including the owner and availability,
  /// allowing users to list and inspect the models they have access to.
  /// </summary>
  /// <remarks>
  /// This class serves as a container for multiple `TModel` instances, each representing
  /// a different model. It is primarily used to retrieve a comprehensive list of models
  /// from the MistralAI API, and it can be helpful for applications that need to display
  /// or manage multiple models at once.
  /// </remarks>
  TModels = MistralAI.Models.TModels;

  /// <summary>
  /// The TModelDeletion class manages the data returned after the successful deletion
  /// of a fine-tuned model in the MistralAI environment. It contains information
  /// about the model that was deleted, such as its ID and deletion status.
  /// </summary>
  /// <remarks>
  /// This class is used to capture the result of a model deletion request.
  /// It provides confirmation that the specified model has been successfully deleted
  /// from the system. This information is important for auditing and tracking purposes,
  /// especially in environments where model management and lifecycle tracking are critical.
  /// </remarks>
  TModelDeletion = MistralAI.Models.TModelDeletion;

  /// <summary>
  /// The TArchivingModel class represents the state of a fine-tuned model in terms of its
  /// archiving status. It is used to track whether a model has been archived or unarchived
  /// within the MistralAI environment.
  /// </summary>
  /// <remarks>
  /// This class is primarily used for operations that involve archiving or unarchiving models.
  /// Archiving a model makes it inactive, while unarchiving restores it to an active state.
  /// The class provides properties to check the current state and to identify the model being
  /// affected.
  /// </remarks>
  TArchivingModel = MistralAI.Models.TArchivingModel;

  /// <summary>
  /// Represents an asynchronous callback parameter for retrieving a list of models.
  /// </summary>
  /// <remarks>
  /// This type is used when performing asynchronous operations to list models in the
  /// MistralAI environment. It enables the handling of the response containing a collection
  /// of models through a callback mechanism, facilitating non-blocking data retrieval.
  /// </remarks>
  TAsynModels = MistralAI.Models.TAsynModels;

  /// <summary>
  /// Represents an asynchronous callback parameter for model deletion operations.
  /// </summary>
  /// <remarks>
  /// This type is used when performing asynchronous delete operations on a model. It
  /// allows the handling of the response which indicates the status of the deletion request.
  /// This type is essential for managing deletion results in a non-blocking manner, enabling
  /// efficient UI updates or further processing based on the deletion status.
  /// </remarks>
  TAsynModelDeletion = MistralAI.Models.TAsynModelDeletion;

  /// <summary>
  /// Represents an asynchronous callback parameter for retrieving details of a specific model.
  /// </summary>
  /// <remarks>
  /// This type is used when performing asynchronous operations to fetch detailed information
  /// about a specific model. It allows for handling the response containing the model's metadata
  /// and capabilities in a non-blocking fashion. This is particularly useful for updating UI elements
  /// or triggering additional actions based on the model's properties.
  /// </remarks>
  TAsynModel = MistralAI.Models.TAsynModel;

  /// <summary>
  /// Represents an asynchronous callback parameter for updating a fine-tuned model.
  /// </summary>
  /// <remarks>
  /// This type is used during asynchronous update operations on fine-tuned models. It enables
  /// handling the response, which contains the updated model details, through a callback mechanism.
  /// This is useful for reflecting changes in the user interface or performing further processing
  /// based on the updated model information.
  /// </remarks>
  TAsynFineTuneModel = MistralAI.Models.TAsynFineTuneModel;

  /// <summary>
  /// Represents an asynchronous callback parameter for archiving or unarchiving a model.
  /// </summary>
  /// <remarks>
  /// This type is used when performing asynchronous archiving or unarchiving operations on a model.
  /// It allows handling the response, which indicates the status of the operation, in a non-blocking manner.
  /// This type is essential for updating the state of the model in the application without blocking the main thread.
  /// </remarks>
  TAsynArchivingModel = MistralAI.Models.TAsynArchivingModel;

  {$ENDREGION}

  {$REGION 'MistralAI.FineTunings'}

  /// <summary>
  /// Represents the parameters for listing fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// Allows filtering and pagination of fine-tuning jobs when retrieving them via the API.
  /// </remarks>
  TFineTuningJobListParams = MistralAI.FineTunings.TFineTuningJobListParams;

  /// <summary>
  /// Represents the hyperparameters used in a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Includes settings such as the number of training steps and the learning rate.
  /// </remarks>
  THyperparametersParams = MistralAI.FineTunings.THyperparametersParams;

  /// <summary>
  /// Represents the integration parameters for a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Specifies details for integrating with external platforms for monitoring fine-tuning jobs, such as Weights and Biases.
  /// </remarks>
  TJobIntegrationsParams = MistralAI.FineTunings.TJobIntegrationsParams;

  /// <summary>
  /// Represents the parameters required for configuring a repository in fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// This class is used to specify details about a repository, including its type, name, owner, reference,
  /// weight, and token. These parameters are typically used when setting up repositories for fine-tuning jobs.
  /// </remarks>
  TRepositoryParams = MistralAI.FineTunings.TRepositoryParams;

  /// <summary>
  /// Represents the parameters for creating a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Includes settings such as the model to fine-tune, training files, hyperparameters, and integrations.
  /// </remarks>
  TFineTuningJobParams = MistralAI.FineTunings.TFineTuningJobParams;

  /// <summary>
  /// Represents the hyperparameters output in a fine-tuning job response.
  /// </summary>
  /// <remarks>
  /// Contains the hyperparameter settings used during fine-tuning.
  /// </remarks>
  TJobOutHyperparameters = MistralAI.FineTunings.TJobOutHyperparameters;

  /// <summary>
  /// Represents the integrations output in a fine-tuning job response.
  /// </summary>
  /// <remarks>
  /// Contains information about integrations enabled for the fine-tuning job.
  /// </remarks>
  TJobOutIntegrations = MistralAI.FineTunings.TJobOutIntegrations;

  /// <summary>
  /// Represents a repository used in the context of fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// This class encapsulates information about a repository, including its type, name, owner, reference, weight, and commit ID.
  /// It is used to provide detailed metadata about the repository in fine-tuning operations.
  /// </remarks>
  TRepository = MistralAI.FineTunings.TRepository;

  /// <summary>
  /// Represents metadata associated with data used in fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// This class provides detailed information about the cost, duration, and token usage
  /// for fine-tuning operations, offering insights into resource consumption.
  /// </remarks>
  TDataMetadata = MistralAI.FineTunings.TDataMetadata;

  /// <summary>
  /// Represents the output of a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Contains details about the fine-tuning job, including its status, parameters, and results.
  /// </remarks>
  TJobOut = MistralAI.FineTunings.TJobOut;

  /// <summary>
  /// Represents a list of fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// Contains an array of fine-tuning job outputs.
  /// </remarks>
  TListFineTuningJobs = MistralAI.FineTunings.TListFineTuningJobs;

  /// <summary>
  /// Status and error dot a fine-tune job.
  /// </summary>
  TJobOutEventData = MistralAI.FineTunings.TJobOutEventData;

  /// <summary>
  /// Represents an event in the fine-tuning job lifecycle.
  /// </summary>
  /// <remarks>
  /// Contains information about status changes during the job.
  /// </remarks>
  TJobOutEvent = MistralAI.FineTunings.TJobOutEvent;

  /// <summary>
  /// Represents metrics associated with a checkpoint during fine-tuning.
  /// </summary>
  /// <remarks>
  /// Contains loss and accuracy metrics.
  /// </remarks>
  TJobOutMetrics = MistralAI.FineTunings.TJobOutMetrics;

  /// <summary>
  /// Represents a checkpoint during the fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Contains metrics and timing information at a specific step.
  /// </remarks>
  TJobOutCheckpoints = MistralAI.FineTunings.TJobOutCheckpoints;

  /// <summary>
  /// Represents detailed progress information for a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Extends <see cref="TJobOut"/> with events and checkpoints.
  /// </remarks>
  TJobOutProgress = MistralAI.FineTunings.TJobOutProgress;

  /// <summary>
  /// Asynchronous callback parameters for listing fine-tuning jobs.
  /// </summary>
  TAsynListFineTuningJobs = MistralAI.FineTunings.TAsynListFineTuningJobs;

  /// <summary>
  /// Asynchronous callback parameters for fine-tuning job output.
  /// </summary>
  TAsynJobOut = MistralAI.FineTunings.TAsynJobOut;

  /// <summary>
  /// Asynchronous callback parameters for fine-tuning job progress.
  /// </summary>
  TAsynJobOutProgress = MistralAI.FineTunings.TAsynJobOutProgress;

  {$ENDREGION}

  {$REGION 'MistralAI.Codestral'}

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
  TCodestralParams = MistralAI.Codestral.TCodestralParams;

  /// <summary>
  /// Represents the token usage statistics for a request, including the number of tokens used in the prompt,
  /// the completion, and the total number of tokens consumed.
  /// </summary>
  /// <remarks>
  /// The <c>TCodestralUsage</c> class provides information on the number of tokens utilized during a request.
  /// This data is essential for understanding the token cost associated with a request, particularly in contexts
  /// where token-based billing is employed, or for monitoring the model's behavior in terms of input and output sizes.
  /// </remarks>
  TCodestralUsage = MistralAI.Codestral.TCodestralUsage;

  /// <summary>
  /// Represents a message exchanged in a conversation, containing the role of the sender and the message content.
  /// </summary>
  /// <remarks>
  /// The <c>TCodestralMessage</c> class captures the essential details of a message in a chat interaction,
  /// including the role of the sender (e.g., user or assistant) and the content of the message.
  /// This class is fundamental for managing and interpreting the flow of a conversation, providing context on who sent
  /// the message and what was communicated.
  /// </remarks>
  TCodestralMessage = MistralAI.Codestral.TCodestralMessage;

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
  TCodestralChoices = MistralAI.Codestral.TCodestralChoices;

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
  TCodestral = MistralAI.Codestral.TCodestral;

  /// <summary>
  /// Manages asynchronous chat events for a chat request using <c>TCodestral</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynCode</c> type extends the <c>TAsynParams&lt;TCodestral&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynCode = MistralAI.Codestral.TAsynCode;

  /// <summary>
  /// Manages asynchronous streaming chat events for a chat request using <c>TCodestral</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynCodeStream</c> type extends the <c>TAsynStreamParams&lt;TCodestral&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynCodeStream = MistralAI.Codestral.TAsynCodeStream;

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
  TCodestralEvent = MistralAI.Codestral.TCodestralEvent;

  {$ENDREGION}

  {$REGION 'MistralAI.Files'}

  /// <summary>
  /// Parameters for uploading a file.
  /// </summary>
  /// <remarks>
  /// This class is used to construct the multipart form data for file uploads.
  /// Example usage:
  /// <code>
  /// var
  ///   UploadParams: TUploadParams;
  /// begin
  ///   UploadParams := TUploadParams.Create;
  ///   try
  ///     UploadParams.File('path/to/file.jsonl');
  ///     UploadParams.Purpose(TFilePurpose.finetune);
  ///     // Use UploadParams with the API upload method
  ///   finally
  ///     UploadParams.Free;
  ///   end;
  /// end;
  /// </code>
  /// </remarks>
  TUploadParams = MistralAI.Files.TUploadParams;

  /// <summary>
  /// Represents the parameters for listing files with specific criteria.
  /// </summary>
  /// <remarks>
  /// This class is used to configure and pass parameters when listing files from the API.
  /// It provides methods to set pagination, filtering, and searching options.
  /// </remarks>
  TListParams = MistralAI.Files.TListParams;

  /// <summary>
  /// Represents a file in the Mistral AI system.
  /// </summary>
  /// <remarks>
  /// This class contains properties that represent the attributes of a file as returned by the API.
  /// </remarks>
  TFile = MistralAI.Files.TFile;

  /// <summary>
  /// Represents a collection of files.
  /// </summary>
  /// <remarks>
  /// This class is used to hold a list of files as returned by the API.
  /// Example usage:
  /// <code>
  /// var
  ///   Files: TFiles;
  ///   FileItem: TFile;
  /// begin
  ///   Files := FilesRoute.List;
  ///   try
  ///     for FileItem in Files.Data do
  ///     begin
  ///       // Process each file
  ///     end;
  ///   finally
  ///     Files.Free;
  ///   end;
  /// end;
  /// </code>
  /// </remarks>
  TFiles = MistralAI.Files.TFiles;

  /// <summary>
  /// Represents the result of a file deletion operation.
  /// </summary>
  /// <remarks>
  /// Contains information about the deletion status and the ID of the deleted file.
  /// </remarks>
  TDeletedResult = MistralAI.Files.TDeletedResult;

  /// <summary>
  /// Represents a downloaded file, providing functionality to access and save its data.
  /// </summary>
  /// <remarks>
  /// This class encapsulates a downloaded file and provides methods to retrieve its data as a stream
  /// or save it to a file. The file data is expected to be in a base64-encoded format.
  /// </remarks>
  TDownLoadFile = MistralAI.Files.TDownLoadFile;

  /// <summary>
  /// Asynchronous callback parameters for operations returning a single <c>TFile</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TFile</c> instance.
  /// </remarks>
  TAsynFile = MistralAI.Files.TAsynFile;

  /// <summary>
  /// Asynchronous callback parameters for operations returning <c>TFiles</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TFiles</c> instance.
  /// </remarks>
  TAsynFiles = MistralAI.Files.TAsynFiles;

  /// <summary>
  /// Asynchronous callback parameters for file deletion operations.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TDeletedResult</c> instance.
  /// </remarks>
  TAsynFilesDelete = MistralAI.Files.TAsynFilesDelete;

  /// <summary>
  /// Asynchronous callback parameters for file download operations.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TDownLoadFile</c> instance.
  /// </remarks>
  TAsynDownLoadFile = MistralAI.Files.TAsynDownLoadFile;

  {$ENDREGION}

  {$REGION 'MistralAI.Functions.Tools'}

  /// <summary>
  /// Represents a tool used for interacting with chat messages, including the ability to convert
  /// functions to JSON format.
  /// </summary>
  TChatMessageTool = MistralAI.Functions.Tools.TChatMessageTool;

  /// <summary>
  /// Represents the specifics of a called function, including its name and calculated arguments.
  /// </summary>
  TCalledFunctionSpecifics = MistralAI.Functions.Tools.TCalledFunctionSpecifics;

  /// <summary>
  /// Represents a called function, containing its specifics such as name and arguments.
  /// </summary>
  TCalledFunction = MistralAI.Functions.Tools.TCalledFunction;

  {$ENDREGION}

  {$REGION 'MistralAI.Agents'}

  /// <summary>
  /// Alias for <c>TChatMessagePayload</c>, representing an agent message payload.
  /// </summary>
  TAgentMessagePayload = MistralAI.Agents.TAgentMessagePayload;

  /// <summary>
  /// Provides methods to interact with agents, including creating completions and handling asynchronous operations.
  /// </summary>
  /// <remarks>
  /// Use this class to manage agent interactions, including synchronous and asynchronous requests.
  /// </remarks>
  TAgentParams = MistralAI.Agents.TAgentParams;

  {$ENDREGION}

  {$REGION 'MistralAI.Functions.Core'}

  /// <summary>
  /// Interface defining the core structure and functionality of a function in the system.
  /// </summary>
  /// <remarks>
  /// This interface outlines the basic properties and methods that any function implementation must include.
  /// </remarks>
  IFunctionCore = MistralAI.Functions.Core.IFunctionCore;

  /// <summary>
  /// Abstract base class for implementing core function behavior.
  /// </summary>
  /// <remarks>
  /// This class provides basic implementations for some methods and defines the structure that derived classes must follow.
  /// </remarks>
  TFunctionCore = MistralAI.Functions.Core.TFunctionCore;

  {$ENDREGION}

  {$REGION 'MistralAI.Classifiers'}

  /// <summary>
  /// <c>TModerationParams</c> class to manage params for guardrails in a text.
  /// </summary>
  /// <remarks>
  /// The policy threshold is determined based on the optimal performance of oan internal test set.
  /// You can use the raw score or adjust the threshold according to your specific use cases.
  /// </remarks>
  TModerationParams = MistralAI.Classifiers.TModerationParams;

  /// <summary>
  /// <c>TModerationChatParams</c> class to manage params for guardrails in a chat generation.
  /// </summary>
  /// <remarks>
  /// The policy threshold is determined based on the optimal performance of oan internal test set.
  /// You can use the raw score or adjust the threshold according to your specific use cases.
  /// </remarks>
  TModerationChatParams = MistralAI.Classifiers.TModerationChatParams;

  /// <summary>
  /// <c>TModerationCategories</c> class defines the categories for moderation checks.
  /// </summary>
  /// <remarks>
  /// This class provides a structured representation of the moderation categories
  /// used to evaluate the content against predefined guardrails. Each property corresponds
  /// to a specific moderation category and indicates whether the content violates that category.
  /// </remarks>
  TModerationCategories = MistralAI.Classifiers.TModerationCategories;

  /// <summary>
  /// <c>TModerationScores</c> class defines the scoring system for moderation categories.
  /// </summary>
  /// <remarks>
  /// This class provides a structured representation of the scores associated with each moderation category.
  /// Each property contains a numerical value indicating the likelihood that the content violates the corresponding category.
  /// </remarks>
  TModerationScores = MistralAI.Classifiers.TModerationScores;

  /// <summary>
  /// Represents a warning item in the moderation results, containing the category and its associated score.
  /// </summary>
  /// <remarks>
  /// This record is used to store information about a specific moderation category violation,
  /// including the category name and the numerical score indicating the likelihood of the violation.
  /// </remarks>
  TWarningItem = MistralAI.Classifiers.TWarningItem;

  /// <summary>
  /// <c>TModerationResult</c> class represents the results of a moderation check.
  /// </summary>
  /// <remarks>
  /// This class contains information about the moderation categories and their associated scores.
  /// It provides a detailed breakdown of whether content violates predefined moderation categories
  /// and the likelihood of such violations.
  /// </remarks>
  TModerationResult = MistralAI.Classifiers.TModerationResult;

  /// <summary>
  /// <c>TModeration</c> class represents the overall result of a moderation request.
  /// </summary>
  /// <remarks>
  /// This class provides a high-level summary of the moderation operation,
  /// including the request ID, the model used for moderation, and the detailed results
  /// for each category and score. It is the main structure returned by the moderation API.
  /// </remarks>
  TModeration = MistralAI.Classifiers.TModeration;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TModeration</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynModeration</c> type extends the <c>TAsynParams&lt;TModeration&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynModeration = MistralAI.Classifiers.TAsynModeration;

  {$ENDREGION}

  {$REGION 'MistralAI.Batch'}

  /// <summary>
  /// Represents a class for constructing and managing query parameters to list batch jobs in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class provides methods to define filters such as page size, model name, creation date, and batch status,
  /// allowing precise control over the listing of batch jobs.
  /// </remarks>
  TBatchJobListParams = MistralAI.Batch.TBatchJobListParams;

  /// <summary>
  /// Represents a class for managing parameters required to create a batch job for the MistralAI API.
  /// This class, <c>TBatchJobParams</c>, extends <c>TJSONParam</c> to construct and handle JSON payloads for batch job creation.
  ///</summary>
  ///<remarks>
  /// A batch job consists of multiple API requests, each identified by a unique <c>custom_id</c>.
  /// This class simplifies the process of defining the parameters and generating the required JSON payloads for a batch job.
  ///</remarks>
  TBatchJobParams = MistralAI.Batch.TBatchJobParams;

  /// <summary>
  /// Represents a class for capturing error information related to batch job processing in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class provides details about errors encountered during batch job processing, including error messages and counts.
  /// </remarks>
  TBatchJobListDataError = MistralAI.Batch.TBatchJobListDataError;

  /// <summary>
  /// Represents a batch job in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// A batch job contains details about the processing status, input files, model used, metadata, and
  /// the results of the batch operation.
  /// </remarks>
  TBatchJob = MistralAI.Batch.TBatchJob;

  /// <summary>
  /// Represents a collection of batch jobs in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class contains a list of batch jobs along with metadata about the collection,
  /// such as the total number of jobs and the object type.
  /// </remarks>
  TBatchJobList = MistralAI.Batch.TBatchJobList;

  /// <summary>
  /// Asynchronous callback parameters for operations returning a single <c>TBatchJob</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TBatchJob</c> instance.
  /// </remarks>
  TAsynBatchJob = MistralAI.Batch.TAsynBatchJob;

  /// <summary>
  /// Asynchronous callback parameters for operations returning a single <c>TBatchJobList</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TBatchJobList</c> instance.
  /// </remarks>
  TAsynBatchJobList = MistralAI.Batch.TAsynBatchJobList;

  {$ENDREGION}

implementation

{ TMistralAI }

constructor TMistralAI.Create;
begin
  inherited;
  FAPI := TMistralAIAPI.Create;
end;

procedure TMistralAI.CodestralCheck;
begin
  if not (CodestralSpec in FSpecs) then
    raise Exception.Create(
       'The MistralAI instance cannot manage "Codestral", for this you must indicate '+
       '[CodestralSpec] as a specification when instantiating the TMistralAI type interface:'#13#13+
       '   TMistralAI.Create(''Your key'', [CodestralSpec])');
end;

constructor TMistralAI.Create(const AToken: string; Specs: TSpecs);
begin
  Create;
  Token := AToken;

  {--- Managing specifications for an instance of the class }
  if CodestralSpec in Specs then
    begin
      FSpecs := FSpecs + [CodestralSpec];
      FAPI.BaseUrl := TMistralAIAPI.URL_BASE_CODESTRAL;
    end;
end;

destructor TMistralAI.Destroy;
begin
  FModelsRoute.Free;
  FEmbeddingsRoute.Free;
  FAgentRoute.Free;
  FChatRoute.Free;
  if CodestralSpec in FSpecs then
    FCodestralRoute.Free;
  FFileRoute.Free;
  FFineTuningRoute.Free;
  FClassifiersRoute.Free;
  FBatchRoute.Free;
  FAPI.Free;
  inherited;
end;

function TMistralAI.GetAgentRoute: TAgentRoute;
begin
  if not Assigned(FAgentRoute) then
    FAgentRoute := TAgentRoute.CreateRoute(API);
  Result := FAgentRoute;
end;

function TMistralAI.GetAPI: TMistralAIAPI;
begin
  Result := FAPI;
end;

function TMistralAI.GetBaseUrl: string;
begin
  Result := FAPI.BaseURL;
end;

function TMistralAI.GetBatchRoute: TBatchRoute;
begin
  if not Assigned(FBatchRoute) then
    FBatchRoute := TBatchRoute.CreateRoute(API);
  Result := FBatchRoute;
end;

function TMistralAI.GetChatRoute: TChatRoute;
begin
  if not Assigned(FChatRoute) then
    FChatRoute := TChatRoute.CreateRoute(API);
  Result := FChatRoute;
end;

function TMistralAI.GetClassifiersRoute: TClassifiersRoute;
begin
  if not Assigned(FClassifiersRoute) then
    FClassifiersRoute := TClassifiersRoute.CreateRoute(API);
  Result := FClassifiersRoute;
end;

function TMistralAI.GetCodestralRoute: TCodestralRoute;
begin
  CodestralCheck;
  if not Assigned(FCodestralRoute) then
    FCodestralRoute := TCodestralRoute.CreateRoute(API);
  Result := FCodestralRoute;
end;

function TMistralAI.GetEmbeddingsRoute: TEmbeddingsRoute;
begin
  if not Assigned(FEmbeddingsRoute) then
    FEmbeddingsRoute := TEmbeddingsRoute.CreateRoute(API);
  Result := FEmbeddingsRoute;
end;

function TMistralAI.GetFilesRoute: TFilesRoute;
begin
  if not Assigned(FFileRoute) then
    FFileRoute := TFilesRoute.CreateRoute(API);
  Result := FFileRoute;
end;

function TMistralAI.GetFineTuningRoute: TFineTuningRoute;
begin
  if not Assigned(FFineTuningRoute) then
    FFineTuningRoute := TFineTuningRoute.CreateRoute(API);
  Result := FFineTuningRoute;
end;

function TMistralAI.GetModelsRoute: TModelsRoute;
begin
  if not Assigned(FModelsRoute) then
    FModelsRoute := TModelsRoute.CreateRoute(API);
  Result := FModelsRoute;
end;

function TMistralAI.GetToken: string;
begin
  Result := FAPI.Token;
end;

procedure TMistralAI.SetBaseUrl(const Value: string);
begin
  FAPI.BaseURL := Value;
end;

procedure TMistralAI.SetToken(const Value: string);
begin
  FAPI.Token := Value;
end;

{ TMistralAIFactory }

class function TMistralAIFactory.CreateInstance(const AToken: string;
  Specs: TSpecs): IMistralAI;
begin
  Result := TMistralAI.Create(AToken, Specs);
end;

end.
