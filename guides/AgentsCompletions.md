# Agents completions (v1/agents/completions)

- [Introduction](#introduction)
- [Agent creation](#agent-Creation)
- [Agent using](#agent-using)

<br>

___

## Introduction

Think of an **AI agent** as an autonomous executor driven by an LLM: you give it a high-level intent, and it orchestrates the necessary steps, selecting and using tools, processing data, and making decisions, to fulfill that intent. Its language comprehension lets it navigate ambiguity and complexity, while multi-agent coordination allows several such executors to collaborate, sequence work, and tackle sophisticated workflows that exceed the scope of any single actor.

<br>

___

## Agent creation

There is no API to create a new agent directly; creation must be done through the platform’s UI at the [designated location](https://console.mistral.ai/build/agents). 

>[!NOTE]
>Agents created this way can be shared in “The Chat.”

<br>

___

## Agent using

The agent’s identifier, available on the platform, is required to initiate its execution.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var Agent_id := 'id'; //e.g. ag:cd365907:20450201:untitled-agent:2be7ed6c

  //Asynchronous promise example
  var Promise := Client.Agent.AsyncAwaitCreateStream(
    procedure (Params: TAgentParams)
    begin
      Params.Messages([
        Payload.User('What is the joual?')
        ]);
      Params.ResponseFormat('text');
      Params.AgentId(Agent_id);
      Params.Stream;
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TPromiseChatStream
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;

      Result.OnProgress :=
        procedure (Sender: TObject; Chunk: TChat)
        begin
          DisplayStream(Sender, Chunk);
        end;

      Result.OnDoCancel := DoCancellation;

      Result.OnCancellation :=
        function (Sender: TObject): string
        begin
          Cancellation(Sender);
        end
    end);

  Promise
    .&Then<string>(
      function (Value: string): string
      begin
        Result := Value;
        ShowMessage(Result);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Agent.AsyncCreateStream(
//    procedure (Params: TAgentParams)
//    begin
//      Params.MaxTokens(1024);
//      Params.Messages([
//        Payload.User('What is the joual?')
//        ]);
//      Params.ResponseFormat('text');
//      Params.AgentId(Agent_id);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynChatStream
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnProgress := DisplayStream;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var WeatherFunc: IFunctionCore := TWeatherReportFunction.Create;
//  var Agent := Client.Agent.Create(
//    procedure (Params: TAgentParams)
//    begin
//      Params.MaxTokens(1024);
//      Params.Messages([
//        Payload.User('What is the joual?')
//        ]);
//      Params.AgentId(Agent_id);
//    end);
//  try
//    Display(TutorialHub, Agent);
//  finally
//    Agent.Free;
//  end;
```

