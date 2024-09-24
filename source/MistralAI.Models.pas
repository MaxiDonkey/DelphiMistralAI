unit MistralAI.Models;

interface

uses
  System.SysUtils, System.Classes, System.Threading, REST.Json.Types,
  MistralAI.API.Params, MistralAI.API, MistralAI.Async.Support;

type
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
  TModelParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets a new name for the fine-tuned model.
    /// </summary>
    /// <param name="Value">The new name to assign to the model.</param>
    /// <returns>Returns the updated TModelParams object.</returns>
    /// <remarks>
    /// The name specified here should be a human-readable identifier for the model.
    /// It is not to be confused with the model's unique ID, which remains constant
    /// and is used for API operations. Changing the name only affects the metadata
    /// of the model.
    /// </remarks>
    function Name(const Value: string): TModelParams;
    /// <summary>
    /// Sets a new description for the fine-tuned model.
    /// </summary>
    /// <param name="Value">The new description to assign to the model.</param>
    /// <returns>Returns the updated TModelParams object.</returns>
    /// <remarks>
    /// Use this method to provide a detailed explanation of the model's purpose,
    /// capabilities, or any other relevant information. This description is useful
    /// for documentation purposes and for understanding the context in which the
    /// model is intended to be used.
    /// </remarks>
    function Description(const Value: string): TModelParams;
  end;

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
  TCapabilities = class
  private
    [JsonNameAttribute('completion_chat')]
    FCompletionChat: Boolean;
    [JsonNameAttribute('completion_fim')]
    FCompletionFim: Boolean;
    [JsonNameAttribute('function_calling')]
    FFunctionCalling: Boolean;
    [JsonNameAttribute('fine_tuning')]
    FFineTuning: Boolean;
  public
    /// <summary>
    /// Indicates whether the model supports chat-based completions.
    /// </summary>
    /// <remarks>
    /// If True, the model can be used in applications that involve conversational
    /// agents or chatbots. This capability is crucial for creating interactive
    /// dialogue systems and enhancing user engagement through natural language
    /// interactions.
    /// </remarks>
    property CompletionChat: Boolean read FCompletionChat write FCompletionChat;
    /// <summary>
    /// Indicates whether the model supports "Fill-in-the-Middle" (FIM) completions.
    /// </summary>
    /// <remarks>
    /// FIM capabilities enable the model to generate text by filling in the blanks
    /// within a given context, which is useful for advanced text editing, code
    /// completion, and other scenarios where context-aware suggestions are needed.
    /// </remarks>
    property CompletionFim: Boolean read FCompletionFim write FCompletionFim;
    /// <summary>
    /// Indicates whether the model supports function calling.
    /// </summary>
    /// <remarks>
    /// If True, the model can trigger and call predefined functions based on
    /// the input it receives. This feature is particularly useful for building
    /// applications that require integration with external services or dynamic
    /// behaviors triggered by user input.
    /// </remarks>
    property FunctionCalling: Boolean read FFunctionCalling write FFunctionCalling;
    /// <summary>
    /// Indicates whether the model supports fine-tuning.
    /// </summary>
    /// <remarks>
    /// Fine-tuning allows the model to be further trained on specific datasets to
    /// improve its performance in specialized tasks. This capability is essential
    /// for customizing the model to better suit particular applications or domains.
    /// </remarks>
    property FineTuning: Boolean read FFineTuning write FFineTuning;
  end;

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
  TCoreModel = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('created')]
    FCreated: Int64;
    [JsonNameAttribute('owned_by')]
    FOwned_by: string;
    [JsonNameAttribute('root')]
    FRoot: string;
    [JsonNameAttribute('archived')]
    FArchived: Boolean;
    [JsonNameAttribute('name')]
    FName: string;
    [JsonNameAttribute('description')]
    FDescription: string;
    [JsonNameAttribute('capabilities')]
    FCapabilities: TCapabilities;
    [JsonNameAttribute('max_context_length')]
    FMaxContextLength: Int64;
    [JsonNameAttribute('aliases')]
    FAliases: TArray<string>;
  public
    /// <summary>
    /// The unique identifier of the model, used to reference the model in API operations.
    /// </summary>
    /// <remarks>
    /// This ID is a unique string that remains constant throughout the model's lifecycle.
    /// It is used to identify the model in API calls such as retrieving details or performing
    /// actions like updating or deleting the model.
    /// </remarks>
    property Id: string read FId write FId;
    /// <summary>
    /// The type of the object, which is always set to "model" for model instances.
    /// </summary>
    /// <remarks>
    /// This property indicates the type of object being represented. It is used internally
    /// by the API to differentiate between different types of entities.
    /// </remarks>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The Unix timestamp (in seconds) representing when the model was created.
    /// </summary>
    /// <remarks>
    /// This timestamp is useful for tracking the model's age and for auditing purposes.
    /// It can be used to determine when the model was first made available in the MistralAI
    /// environment.
    /// </remarks>
    property Created: Int64 read FCreated write FCreated;
    /// <summary>
    /// The owner of the model, typically represented as a user or organization.
    /// </summary>
    /// <remarks>
    /// This property shows who has ownership of the model. Ownership can affect
    /// permissions and accessibility, especially in shared environments.
    /// </remarks>
    property OwnedBy: string read FOwned_by write FOwned_by;
    /// <summary>
    /// The root identifier of the model, indicating its base version or lineage.
    /// </summary>
    /// <remarks>
    /// The root ID is used to track the lineage of the model, particularly when dealing
    /// with versions or fine-tuned variants derived from a base model. It helps in
    /// understanding the ancestry and evolution of the model.
    /// </remarks>
    property Root: string read FRoot write FRoot;
    /// <summary>
    /// Indicates whether the model is archived.
    /// </summary>
    /// <remarks>
    /// If True, the model is archived and is not actively available for use. Archived
    /// models are typically preserved for record-keeping or future reactivation but
    /// cannot be used in regular operations unless unarchived.
    /// </remarks>
    property Archived: Boolean read FArchived write FArchived;
    /// <summary>
    /// The name of the model, which is used for display and identification purposes.
    /// </summary>
    /// <remarks>
    /// The name is a human-readable string that helps to identify the model. It is not
    /// unique and can be modified as needed without affecting the model's functionality.
    /// </remarks>
    property Name: string read FName write FName;
    /// <summary>
    /// A detailed description of the model, providing information about its purpose, capabilities, and other relevant details.
    /// </summary>
    /// <remarks>
    /// The description helps users understand what the model is designed to do, its intended
    /// use cases, and any special characteristics. It is especially useful for documentation
    /// and communication within teams.
    /// </remarks>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// Represents the capabilities of the model, such as whether it supports fine-tuning or chat-based completion.
    /// </summary>
    /// <remarks>
    /// The capabilities provide a quick overview of what the model can do. Each capability is
    /// represented as a boolean value, indicating whether the model supports that feature. This
    /// information is essential for determining the suitability of the model for specific tasks.
    /// </remarks>
    property Capabilities: TCapabilities read FCapabilities write FCapabilities;
    /// <summary>
    /// The maximum context length that the model can handle.
    /// </summary>
    /// <remarks>
    /// This value represents the maximum number of tokens or characters that the model can process
    /// in a single request. It is an important parameter to consider when working with large inputs,
    /// as exceeding this limit may result in truncated responses or errors.
    /// </remarks>
    property MaxContextLength: Int64 read FMaxContextLength write FMaxContextLength;
    /// <summary>
    /// An array of alternative names or aliases for the model.
    /// </summary>
    /// <remarks>
    /// Aliases provide additional identifiers for the model, which can be useful in scenarios
    /// where multiple names are used to reference the same model. This property helps in
    /// maintaining consistency and flexibility in model references.
    /// </remarks>
    property Aliases: TArray<string> read FAliases write FAliases;
    /// <summary>
    /// Destructor to clean up allocated resources.
    /// </summary>
    /// <remarks>
    /// This destructor ensures that all dynamically allocated objects and resources associated
    /// with the TCoreModel instance are properly released, preventing memory leaks.
    /// </remarks>
    destructor Destroy; override;
  end;

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
  TModel = class(TCoreModel)
  private
    [JsonNameAttribute('deprecation')]
    FDeprecation: string;
  public
    /// <summary>
    /// Indicates the deprecation date of the model as a string.
    /// </summary>
    /// <remarks>
    /// This property provides the date or version at which the model was deprecated.
    /// Deprecation means that the model is no longer recommended for use and may be
    /// removed or become unsupported in the future. Applications using deprecated
    /// models should plan to transition to updated models to maintain compatibility
    /// and support.
    /// </remarks>
    property Deprecation: string read FDeprecation write FDeprecation;
  end;

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
  TFineTunedModel = class(TCoreModel)
  private
    [JsonNameAttribute('job')]
    FJob: string;
  public
    /// <summary>
    /// The identifier of the job associated with the fine-tuning process.
    /// </summary>
    /// <remarks>
    /// The Job property represents the ID of the fine-tuning task that created or modified
    /// the model. This ID can be used to track the details of the fine-tuning operation,
    /// such as the dataset used, training parameters, and performance metrics. It is
    /// particularly useful for auditing purposes and for understanding the history of the model.
    /// </remarks>
    property Job: string read FJob write FJob;
  end;

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
  TModels = class
  private
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('data')]
    FData: TArray<TModel>;
  public
    /// <summary>
    /// The type of the object, typically "list" for collections.
    /// </summary>
    /// <remarks>
    /// This property indicates the type of the object represented by the class instance.
    /// For collections like `TModels`, it is usually set to "list", signifying that the
    /// object contains a list of other objects, in this case, models.
    /// </remarks>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// An array of `TModel` objects, each representing an individual model.
    /// </summary>
    /// <remarks>
    /// This property contains the data returned by the API, which is a list of models
    /// that are currently available. Each model in the array provides detailed information
    /// about its properties, capabilities, and ownership. This property is essential for
    /// accessing the complete set of models in a structured format.
    /// </remarks>
    property Data: TArray<TModel> read FData write FData;
    /// <summary>
    /// Destructor to clean up allocated resources.
    /// </summary>
    /// <remarks>
    /// This destructor ensures that all dynamically allocated objects and resources associated
    /// with the `TModels` instance are properly released, preventing memory leaks.
    /// It also frees each `TModel` object contained in the `Data` array.
    /// </remarks>
    destructor Destroy; override;
  end;

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
  TModelDeletion = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('deleted')]
    FDeleted: Boolean;
  public
    /// <summary>
    /// The ID of the fine-tuned model that was deleted.
    /// </summary>
    /// <remarks>
    /// This ID uniquely identifies the model that was removed from the system.
    /// It can be used to verify which model was deleted, particularly when managing
    /// multiple models or when logging deletion activities for compliance purposes.
    /// </remarks>
    property Id: string read FId write FId;
    /// <summary>
    /// The type of the object that was deleted, which is always "model" for model deletions.
    /// </summary>
    /// <remarks>
    /// This property confirms that the deleted object was a model. It is used internally
    /// by the API to verify the type of entity that was removed, ensuring that the correct
    /// object type was deleted.
    /// </remarks>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// Indicates whether the model was successfully deleted.
    /// </summary>
    /// <remarks>
    /// If True, the model has been successfully deleted from the MistralAI environment.
    /// This flag serves as confirmation that the deletion operation completed without
    /// errors, and the model is no longer available in the system.
    /// </remarks>
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

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
  TArchivingModel = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('archived')]
    FArchived: Boolean;
  public
    /// <summary>
    /// The unique identifier of the fine-tuned model to be archived or unarchived.
    /// </summary>
    /// <remarks>
    /// This ID is used to reference the model in archiving operations. It is essential
    /// for identifying the specific model whose state is being changed. Ensure that the
    /// correct ID is provided when performing archiving or unarchiving actions to avoid
    /// unintentional changes to other models.
    /// </remarks>
    property Id: string read FId write FId;
    /// <summary>
    /// The type of the object, which is always set to "model" for models being archived or unarchived.
    /// </summary>
    /// <remarks>
    /// This property indicates the type of the entity involved in the operation. For all models,
    /// this value will be "model", confirming that the object being archived or unarchived is indeed
    /// a model and not another type of entity.
    /// </remarks>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// Indicates whether the model is currently archived.
    /// </summary>
    /// <remarks>
    /// If True, the model is archived and not available for use. If False, the model is active and
    /// can be used normally. This property helps in tracking the current state of the model and is
    /// particularly useful for managing large sets of models where some may be inactive.
    /// </remarks>
    property Archived: Boolean read FArchived write FArchived;
  end;

  /// <summary>
  /// Represents an asynchronous callback parameter for retrieving a list of models.
  /// </summary>
  /// <remarks>
  /// This type is used when performing asynchronous operations to list models in the
  /// MistralAI environment. It enables the handling of the response containing a collection
  /// of models through a callback mechanism, facilitating non-blocking data retrieval.
  /// </remarks>
  TAsynModels = TAsyncCallBack<TModels>;

  /// <summary>
  /// Represents an asynchronous callback parameter for model deletion operations.
  /// </summary>
  /// <remarks>
  /// This type is used when performing asynchronous delete operations on a model. It
  /// allows the handling of the response which indicates the status of the deletion request.
  /// This type is essential for managing deletion results in a non-blocking manner, enabling
  /// efficient UI updates or further processing based on the deletion status.
  /// </remarks>
  TAsynModelDeletion = TAsyncCallBack<TModelDeletion>;

  /// <summary>
  /// Represents an asynchronous callback parameter for retrieving details of a specific model.
  /// </summary>
  /// <remarks>
  /// This type is used when performing asynchronous operations to fetch detailed information
  /// about a specific model. It allows for handling the response containing the model's metadata
  /// and capabilities in a non-blocking fashion. This is particularly useful for updating UI elements
  /// or triggering additional actions based on the model's properties.
  /// </remarks>
  TAsynModel = TAsyncCallBack<TModel>;

  /// <summary>
  /// Represents an asynchronous callback parameter for updating a fine-tuned model.
  /// </summary>
  /// <remarks>
  /// This type is used during asynchronous update operations on fine-tuned models. It enables
  /// handling the response, which contains the updated model details, through a callback mechanism.
  /// This is useful for reflecting changes in the user interface or performing further processing
  /// based on the updated model information.
  /// </remarks>
  TAsynFineTuneModel = TAsyncCallBack<TFineTunedModel>;

  /// <summary>
  /// Represents an asynchronous callback parameter for archiving or unarchiving a model.
  /// </summary>
  /// <remarks>
  /// This type is used when performing asynchronous archiving or unarchiving operations on a model.
  /// It allows handling the response, which indicates the status of the operation, in a non-blocking manner.
  /// This type is essential for updating the state of the model in the application without blocking the main thread.
  /// </remarks>
  TAsynArchivingModel = TAsyncCallBack<TArchivingModel>;

  /// <summary>
  /// The TModelsRoute class provides various methods for managing Large Language Models (LLMs)
  /// through MistralAI's API. It offers both synchronous and asynchronous methods to list, retrieve,
  /// delete, update, archive, and un-archive models.
  /// </summary>
  /// <remarks>
  /// This class is designed to interact with the MistralAI API to facilitate management operations
  /// on models. It supports fine-tuning models as well as handling models' metadata and status.
  /// Each method provides detailed interaction with the API, ensuring efficient model management.
  /// </remarks>
  TModelsRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Asynchronously lists the currently available models.
    /// </summary>
    /// <param name="CallBacks">A callback function that receives the list of models asynchronously.</param>
    /// <remarks>
    /// Use this method to get a list of all models available in the MistralAI environment.
    /// This is useful for obtaining an overview of the models you have access to.
    ///
    /// <code>
    ///  MistralAI.Models.AsyncList(
    ///     function : TAsynModels
    ///     begin
    ///       Result.OnSuccess :=
    ///         procedure (Sender: TObject; List: TModels)
    ///         begin
    ///           // Handle List
    ///         end
    ///     end);
    /// </code>
    /// </remarks>
    procedure AsyncList(const CallBacks: TFunc<TAsynModels>);
    /// <summary>
    /// Asynchronously deletes a fine-tuned model.
    /// </summary>
    /// <param name="ModelId">The ID of the fine-tuned model to be deleted.</param>
    /// <param name="CallBacks">A callback function that receives the deletion status asynchronously.</param>
    /// <remarks>
    /// Deleting a model is a permanent action. Use this method with caution, as it will remove
    /// the model and its metadata from the MistralAI environment.
    ///
    /// <code>
    ///  MistralAI.Models.AsyncDelete( ModelId,
    ///     function : TAsynModelDeletion
    ///     begin
    ///       Result.OnSuccess :=
    ///         procedure (Sender: TObject; Model: TModels)
    ///         begin
    ///           //Handle success
    ///         end
    ///     end);
    /// </code>
    /// </remarks>
    procedure AsyncDelete(const ModelId: string; const CallBacks: TFunc<TAsynModelDeletion>);
    /// <summary>
    /// Asynchronously retrieves the details of a model.
    /// </summary>
    /// <param name="ModelId">The ID of the model to be retrieved.</param>
    /// <param name="CallBacks">A callback function that receives the model details asynchronously.</param>
    /// <remarks>
    /// This method is useful for fetching detailed information about a specific model,
    /// including its metadata, capabilities, and current status.
    ///
    /// <code>
    ///  MistralAI.Models.AsyncRetrieve( ModelId,
    ///     function : TAsynModel
    ///     begin
    ///       Result.OnSuccess :=
    ///         procedure (Sender: TObject; Model: TModel)
    ///         begin
    ///           //Handle success
    ///         end
    ///     end);
    /// </code>
    /// </remarks>
    procedure AsyncRetrieve(const ModelId: string; const CallBacks: TFunc<TAsynModel>);
    /// <summary>
    /// Asynchronously updates the details of a fine-tuned model.
    /// </summary>
    /// <param name="ModelId">The ID of the fine-tuned model to be updated.</param>
    /// <param name="ParamProc">A procedure that specifies the parameters to be updated.</param>
    /// <param name="CallBacks">A callback function that receives the updated model details asynchronously.</param>
    /// <remarks>
    /// Use this method to change the name or description of a fine-tuned model. Ensure that the
    /// provided parameters are valid, as incorrect data might result in unexpected behavior.
    ///
    /// <code>
    ///  MistralAI.Models.AsyncUpdate( ModelId,
    ///     procedure (Params: TModelParams)
    ///     begin
    ///       // Define updating params
    ///     end,
    ///     function : TAsynFineTuneModel
    ///     begin
    ///       Result.OnSuccess :=
    ///         procedure (Sender: TObject; Model: TFineTunedModel)
    ///         begin
    ///           //Handle success
    ///         end
    ///     end);
    /// </code>
    /// </remarks>
    procedure AsyncUpdate(const ModelId: string; ParamProc: TProc<TModelParams>;
      const CallBacks: TFunc<TAsynFineTuneModel>);
    /// <summary>
    /// Asynchronously archives a fine-tuned model.
    /// </summary>
    /// <param name="ModelId">The ID of the fine-tuned model to be archived.</param>
    /// <param name="CallBacks">A callback function that receives the archiving status asynchronously.</param>
    /// <remarks>
    /// Archiving a model will make it unavailable for use. This is typically used to manage
    /// storage or to keep the model's state intact while it is not actively in use.
    ///
    /// <code>
    ///  MistralAI.Models.AsyncArchive( ModelId,
    ///     function : TAsynArchivingModel
    ///     begin
    ///       Result.OnSuccess :=
    ///         procedure (Sender: TObject; Model: TArchivingModel)
    ///         begin
    ///           //Handle success
    ///         end
    ///     end);
    /// </code>
    /// </remarks>
    procedure AsyncArchive(const ModelId: string; const CallBacks: TFunc<TAsynArchivingModel>);
    /// <summary>
    /// Asynchronously un-archives a fine-tuned model.
    /// </summary>
    /// <param name="ModelId">The ID of the fine-tuned model to be un-archived.</param>
    /// <param name="CallBacks">A callback function that receives the un-archiving status asynchronously.</param>
    /// <remarks>
    /// Un-archiving a model will restore it to an active state, making it available for use again.
    /// This is typically used for bringing back models that were previously archived.
    ///
    ///  <code>
    ///  MistralAI.Models.AsyncUnarchive( ModelId,
    ///     function : TAsynArchivingModel
    ///     begin
    ///       Result.OnSuccess :=
    ///         procedure (Sender: TObject; Model: TArchivingModel)
    ///         begin
    ///           //Handle success
    ///         end
    ///     end);
    /// </code>
    /// </remarks>
    procedure AsyncUnarchive(const ModelId: string; const CallBacks: TFunc<TAsynArchivingModel>);
    /// <summary>
    /// Lists the currently available models.
    /// </summary>
    /// <returns>Returns a list of currently available models.</returns>
    /// <remarks>
    /// This method provides a synchronous way to fetch all models in the MistralAI environment.
    /// It is useful for applications where synchronous data access is required.
    /// <code>
    ///   var Models := MistralAI.Models.List;
    ///   try
    ///     // List processing
    ///   finally
    ///     Models.Free;
    ///   end;
    /// </code>
    /// </remarks>
    function List: TModels;
    /// <summary>
    /// Deletes a fine-tuned model.
    /// </summary>
    /// <param name="ModelId">The ID of the fine-tuned model to be deleted.</param>
    /// <returns>Returns the deletion status of the model.</returns>
    /// <remarks>
    /// Deleting a fine-tuned model removes it permanently from the MistralAI environment.
    /// This action cannot be undone, so use it with caution.
    /// <code>
    ///   with MistralAI.Models.Delete('ModelId');
    ///   try
    ///     if Deleted then
    ///       WriteLn('Modèle deleted');
    ///   finally
    ///     Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Delete(const ModelId: string): TModelDeletion;
    /// <summary>
    /// Retrieves the details of a model.
    /// </summary>
    /// <param name="ModelId">The ID of the model to be retrieved.</param>
    /// <returns>Returns the details of the specified model.</returns>
    /// <remarks>
    /// This method is used to access detailed information about a specific model, including its
    /// creation date, owner, and capabilities. This is useful for managing and reviewing model
    /// details.
    /// <code>
    ///   with MistralAI.Models.Retrieve('ModelId');
    ///   try
    ///     // Model found processing
    ///   finally
    ///     Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Retrieve(const ModelId: string): TModel;
    /// <summary>
    /// Updates the details of a fine-tuned model.
    /// </summary>
    /// <param name="ModelId">The ID of the fine-tuned model to be updated.</param>
    /// <param name="ParamProc">A procedure that specifies the parameters to be updated.</param>
    /// <returns>Returns the updated model details.</returns>
    /// <remarks>
    /// Use this method to change specific details of a fine-tuned model, such as its name or
    /// description. This is useful for managing model metadata.
    /// <code>
    ///   var FineTuned := MistralAI.Models.Retrieve(
    ///          'ModelId',
    ///          procedure (Params: TModelParams)
    ///          begin
    ///            // Define params to retrieve the model
    ///          end);
    ///   if Assigned(FineTuned) then
    ///     try
    ///       // Handle FineTuned.job
    ///     finally
    ///       FineTuned.Free;
    ///     end;
    /// </code>
    /// </remarks>
    function Update(const ModelId: string; ParamProc: TProc<TModelParams>): TFineTunedModel;
    /// <summary>
    /// Archives a fine-tuned model.
    /// </summary>
    /// <param name="ModelId">The ID of the fine-tuned model to be archived.</param>
    /// <returns>Returns the archiving status of the model.</returns>
    /// <remarks>
    /// Archiving a model makes it unavailable for use but preserves its state and metadata.
    /// This is useful for managing storage and ensuring that models are not used when they are
    /// not needed.
    /// <code>
    ///   with MistralAI.Models.Archive('ModelId');
    ///   try
    ///     // Model is archived
    ///   finally
    ///     Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Archive(const ModelId: string): TArchivingModel;
    /// <summary>
    /// Un-archives a fine-tuned model.
    /// </summary>
    /// <param name="ModelId">The ID of the fine-tuned model to be un-archived.</param>
    /// <returns>Returns the un-archiving status of the model.</returns>
    /// <remarks>
    /// Un-archiving a model restores it to an active state, making it available for use again.
    /// This is useful for reactivating models that were previously archived.
    /// <code>
    ///   with MistralAI.Models.Unarchive('ModelId');
    ///   try
    ///     // Model is unarchived
    ///   finally
    ///     Free;
    ///   end;
    /// </remarks>
    function Unarchive(const ModelId: string): TArchivingModel;
  end;

implementation

{ TModels }

destructor TModels.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TModelsRoute }

function TModelsRoute.Archive(const ModelId: string): TArchivingModel;
begin
  Result := API.Post<TArchivingModel>(Format('fine_tuning/models/%s/archive', [ModelId]));
end;

procedure TModelsRoute.AsyncArchive(const ModelId: string;
  const CallBacks: TFunc<TAsynArchivingModel>);
begin
  with TAsyncCallBackExec<TAsynArchivingModel, TArchivingModel>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TArchivingModel
      begin
        Result := Archive(ModelId);
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsyncDelete(const ModelId: string;
  const CallBacks: TFunc<TAsynModelDeletion>);
begin
  with TAsyncCallBackExec<TAsynModelDeletion, TModelDeletion>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModelDeletion
      begin
        Result := Delete(ModelId);
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsyncList(const CallBacks: TFunc<TAsynModels>);
begin
  with TAsyncCallBackExec<TAsynModels, TModels>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModels
      begin
        Result := List;
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsyncRetrieve(const ModelId: string;
  const CallBacks: TFunc<TAsynModel>);
begin
  with TAsyncCallBackExec<TAsynModel, TModel>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModel
      begin
        Result := Retrieve(ModelId);
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsyncUnarchive(const ModelId: string;
  const CallBacks: TFunc<TAsynArchivingModel>);
begin
  with TAsyncCallBackExec<TAsynArchivingModel, TArchivingModel>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TArchivingModel
      begin
        Result := Unarchive(ModelId);
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsyncUpdate(const ModelId: string;
  ParamProc: TProc<TModelParams>;
  const CallBacks: TFunc<TAsynFineTuneModel>);
begin
  with TAsyncCallBackExec<TAsynFineTuneModel, TFineTunedModel>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFineTunedModel
      begin
        Result := Update(ModelId, ParamProc);
      end);
  finally
    Free;
  end;
end;

function TModelsRoute.Delete(const ModelId: string): TModelDeletion;
begin
  Result := API.Delete<TModelDeletion>(Format('models/%s', [ModelId]));
end;

function TModelsRoute.List: TModels;
begin
  Result := API.Get<TModels>('models');
end;

function TModelsRoute.Retrieve(const ModelId: string): TModel;
begin
  Result := API.Get<TModel>(Format('models/%s', [ModelId]));
end;

function TModelsRoute.Unarchive(const ModelId: string): TArchivingModel;
begin
  Result := API.Delete<TArchivingModel>(Format('fine_tuning/models/%s/archive', [ModelId]));
end;

function TModelsRoute.Update(const ModelId: string;
  ParamProc: TProc<TModelParams>): TFineTunedModel;
begin
  Result := API.Post<TFineTunedModel, TModelParams>(Format('fine_tuning/models/%s', [ModelId]), ParamProc);
end;

{ TCoreModel }

destructor TCoreModel.Destroy;
begin
  if Assigned(FCapabilities) then
    FCapabilities.Free;
  inherited;
end;

{ TModelParams }

function TModelParams.Description(const Value: string): TModelParams;
begin
  Result := TModelParams(Add('description', Value));
end;

function TModelParams.Name(const Value: string): TModelParams;
begin
  Result := TModelParams(Add('name', Value));
end;

end.
