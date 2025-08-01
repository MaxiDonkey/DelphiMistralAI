# Moderation 

- [Moderation API](#moderation-api)
- [Validation and security system layer](#validation-and-security-system-layer)

<br>

___

## Moderation API

The moderation service, leveraging the advanced Mistral Moderation model—a classifier built on the Ministral 8B 24.10 framework—empowers users to identify harmful text content across multiple policy dimensions.

- [Raw-text endpoint](#raw-text-endpoint)
- [Conversational endpoint](#conversational-endpoint)
- [Managing moderation categories](#managing-moderation-categories)

<br>

___

### Raw-text endpoint

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  var text_to_classify := 'text_to_classify';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Classifiers.AsyncAwaitModeration(
    procedure (Params: TModerationParams)
    begin
      Params.Model('mistral-moderation-latest');
      Params.Input(text_to_classify);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<TModeration>(
      function (Value: TModeration): TModeration
      begin
        Result := Value;
        DisplayEx(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Classifiers.ASyncModeration(
//    procedure (Params: TModerationParams)
//    begin
//      Params.Model('mistral-moderation-latest');
//      Params.Input(text_to_classify);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynModeration
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnSuccess := DisplayEx;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Moderation := Client.Classifiers.Moderation(
//    procedure (Params: TModerationParams)
//    begin
//      Params.Model('mistral-moderation-latest');
//      Params.Input(text_to_classify);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Moderation);
//  finally
//    Moderation.Free;
//  end;
```

<br>

___

### Conversational endpoint

For conversational use of the moderation API, it's best to call the conversational endpoint and submit your dialogue as illustrated below. The model evaluates the most recent turn in the context of the preceding conversation.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var text_to_classify := 'text_to_classify';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Classifiers.AsyncAwaitModerationChat(
    procedure (Params: TModerationChatParams)
    begin
      Params.Model('mistral-moderation-latest');
      Params.Input([Payload.User(text_to_classify)]);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<TModeration>(
      function (Value: TModeration): TModeration
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
//  Client.Classifiers.ASyncModerationChat(
//    procedure (Params: TModerationChatParams)
//    begin
//      Params.Model('mistral-moderation-latest');
//      Params.Input([Payload.User(text_to_classify)]);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynModeration
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Moderation := Client.Classifiers.ModerationChat(
//    procedure (Params: TModerationChatParams)
//    begin
//      Params.Model('mistral-moderation-latest');
//      Params.Input([Payload.User(text_to_classify)]);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Moderation);
//  finally
//    Moderation.Free;
//  end;
```

<br>

___

### Managing moderation categories

Moderation categories can be easily customized for simplified display. The examples below demonstrate how this can be achieved through code.

<br>

**Show all categories with each associated score.**

```Pascal
procedure Display(Sender: TObject; Value: TModerationResult);
begin
  if Value.Warning then
    begin
      Display(Sender, [EmptyStr,
        F(Classifiers[0], Value.Categories.Sexual, Value.Scores.Sexual),
        F(Classifiers[1], Value.Categories.Hate_and_discrimination, Value.Scores.Hate_and_discrimination),
        F(Classifiers[2], Value.Categories.Violence_and_threats, Value.Scores.Violence_and_threats),
        F(Classifiers[3], Value.Categories.Dangerous_and_criminal_content, Value.Scores.Dangerous_and_criminal_content),
        F(Classifiers[4], Value.Categories.Selfharm, Value.Scores.Selfharm),
        F(Classifiers[5], Value.Categories.Health, Value.Scores.Health),
        F(Classifiers[6], Value.Categories.Financial, Value.Scores.Financial),
        F(Classifiers[7], Value.Categories.Law, Value.Scores.Law),
        F(Classifiers[8], Value.Categories.Pii, Value.Scores.Pii)
      ]);
    end
  else
    begin
      Display(Sender, 'No moderation is necessary');
    end;
end;
```

<br>

**Show categories with a non-zero score.**

```Pascal
procedure DisplayEx(Sender: TObject; Value: TModerationResult);
var
  Temp: TArray<string>;
begin
  if Value.Warning then
    begin
      Temp := [''];
      for var Item in Value.Warnings do
        Temp := Temp + [F(Item.Category, (Item.Score * 100).ToString(ffNumber, 3, 2)) + '%'];
      Display(Sender, Temp);
      Display(Sender);
    end
  else
    begin
      Display(Sender, 'No moderation is necessary');
    end;
end;
```

<br>

___

## Validation and security system layer

Enforcing guardrails in chat responses is essential for user-facing applications. To support this, an optional system prompt designed to apply safety constraints on top of the models. This feature can be enabled by setting the `safe_prompt` boolean flag in your API request, as shown below:

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

   var text_to_classify := 'text_to_classify';

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Chat.AsyncAwaitCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-tiny');
      Params.Messages([Payload.User(text_to_classify)]);
      Params.SafePrompt(True);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<string>(
      function (Value: TChat): string
      begin
        for var Item in Value.Choices do
          Result := Result + Item.Message.Content[0].Text;
        Display(TutorialHub, Value);
        ShowMessage(Result);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```
