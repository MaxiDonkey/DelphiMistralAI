# Model Management API (v1/models)

- [Introduction](#introduction)
- [Retrieve the list of models](#retrieve-the-list-of-models)
- [Retrieve a model](#retrieve-a-model)
- [Fine tuned models](#fine-tuned-models)


<br>

___

## Introduction

The current model ecosystem is structured around clearly defined use cases: reasoning, code generation, multimodal processing (text, image, audio), transcription, embeddings, and content moderation. All models are accessible through a unified and versioned API, ensuring long-term stability and traceability while enabling gradual adoption of the latest iterations.

Each model category is designed to address specific constraints: depth of reasoning, context window size, low latency, or multimodal input support. The system follows a continuous update cycle, with frequent releases of improved versions and a controlled deprecation timeline for older ones.

Models are grouped into two main families: proprietary models optimized for industrial-grade performance, and open-source models under the Apache 2 license, allowing flexible integration, including on-prem deployment or custom pipelines. The size granularity (mini, small, medium, large) enables precise tuning of trade-offs between performance, cost, and memory footprint.

Finally, the platform encourages early adoption of new versions while maintaining the backward compatibility required for critical environments. Each model is accessible through a dedicated endpoint, with versioned documentation available to support continuous integration and evolution.

For more details on the available models, please refer to the [Models Documentation](https://docs.mistral.ai/models/)

<br>

___

## Retrieve the list of models

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Models.AsyncAwaitList;

  promise
    .&Then<TModels>(
      function (Value: TModels): TModels
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Models.AsyncList(
//    function : TAsynModels
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Models := Client.Models.List;
//  try
//    Display(TutorialHub, Models);
//  finally
//    Models.Free;
//  end;
```

<br>

___

## Retrieve a model

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Models.AsyncAwaitRetrieve('mistral-large-2407');

  promise
    .&Then<TModel>(
      function (Value: TModel): TModel
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Models.AsyncRetrieve('mistral-large-2407',
//    function : TAsynModel
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Model := Client.Models.Retrieve('mistral-large-2407');
//  try
//    Display(TutorialHub, Model);
//  finally
//    Model.Free;
//  end;
```

<br>

___

## Fine tuned models

Fine-tuned models can be managed through various operations, including deletion, renaming, archiving, and unarchiving. To perform these actions, the following functions are available in both synchronous and asynchronous modes:

**Synchronous Mode:** 
```Pascal
- function Delete(const ModelId: string): TModelDeletion;
- function Update(const ModelId: string; ParamProc: TProc<TModelParams>): TFineTunedModel;
- function Archive(const ModelId: string): TArchivingModel;
- function Unarchive(const ModelId: string): TArchivingModel;
```

**Asynchronous Mode:** 
```Pascal
- procedure AsyncDelete(const ModelId: string; const CallBacks: TFunc<TAsynModelDeletion>);
- procedure AsyncUpdate(const ModelId: string; ParamProc: TProc<TModelParams>;
      const CallBacks: TFunc<TAsynFineTuneModel>);
- procedure AsyncArchive(const ModelId: string; const CallBacks: TFunc<TAsynArchivingModel>);
- procedure AsyncUnarchive(const ModelId: string; const CallBacks: TFunc<TAsynArchivingModel>);
```

These functions provide a flexible and comprehensive approach to maintaining and organizing fine-tuned models.


<br>

___

