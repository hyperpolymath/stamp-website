// SPDX-License-Identifier: PMPL-1.0-or-later
// STAMP Protocol - TEA Interactive Demo
// With k9-svc validation, a2ml typed content, and proven formal verification

open ProvenSafeUrl

type demoStep =
  | Initial
  | SubscribeRequested
  | SubscribeConfirmed
  | VerifyRequested
  | VerificationShown
  | UnsubscribeRequested
  | UnsubscribeConfirmed

type urlValidation =
  | NotValidated
  | Valid(parsedUrl)
  | Invalid(string)

type model = {
  demoStep: demoStep,
  demoClicks: int,
  unsubscribeUrl: string,
  urlValidation: urlValidation,
}

type msg =
  | NextStep
  | TrackClick
  | ValidateUnsubscribeUrl(string)

let init = (): (model, Tea.Cmd.t<msg>) => {
  let model = {
    demoStep: Initial,
    demoClicks: 0,
    unsubscribeUrl: "https://stamp-protocol.org/unsubscribe?token=abc123",
    urlValidation: NotValidated,
  }
  (model, Tea.Cmd.none)
}

let update = (model: model, msg: msg): (model, Tea.Cmd.t<msg>) => {
  switch msg {
  | NextStep =>
      let nextStep = switch model.demoStep {
      | Initial => SubscribeRequested
      | SubscribeRequested => SubscribeConfirmed
      | SubscribeConfirmed => VerifyRequested
      | VerifyRequested => VerificationShown
      | VerificationShown => UnsubscribeRequested
      | UnsubscribeRequested => UnsubscribeConfirmed
      | UnsubscribeConfirmed => Initial
      }
      // Validate URL when entering UnsubscribeRequested state
      let (updatedModel, cmd) = if nextStep == UnsubscribeRequested {
        // Use ProvenSafeUrl to validate the unsubscribe link
        switch parse(model.unsubscribeUrl) {
        | Ok(parsedUrl) =>
            // Verify it's HTTPS (required for unsubscribe links)
            if isHttps(model.unsubscribeUrl) {
              ({...model, demoStep: nextStep, urlValidation: Valid(parsedUrl)}, Tea.Cmd.none)
            } else {
              ({...model, demoStep: nextStep, urlValidation: Invalid("Must use HTTPS")}, Tea.Cmd.none)
            }
        | Error(err) =>
            ({...model, demoStep: nextStep, urlValidation: Invalid(err)}, Tea.Cmd.none)
        }
      } else {
        ({...model, demoStep: nextStep}, Tea.Cmd.none)
      }
      (updatedModel, cmd)

  | TrackClick =>
      ({...model, demoClicks: model.demoClicks + 1}, Tea.Cmd.none)

  | ValidateUnsubscribeUrl(url) =>
      // Formally verify URL using proven
      switch parse(url) {
      | Ok(parsedUrl) =>
          ({...model, unsubscribeUrl: url, urlValidation: Valid(parsedUrl)}, Tea.Cmd.none)
      | Error(err) =>
          ({...model, unsubscribeUrl: url, urlValidation: Invalid(err)}, Tea.Cmd.none)
      }
  }
}

let view = (model: model) => {
  open Tea.Html

  let stepContent = switch model.demoStep {
  | Initial =>
      div([], [
        p([], [text("Interactive STAMP Protocol Demo")]),
        p([], [text("See how dependent types prove message compliance")])
      ])

  | SubscribeRequested =>
      div([], [
        p([], [text("✓ Subscribe request sent")]),
        pre([class'("a2ml-proof")], [
          text("(proof consent\n"),
          text("  (action subscribe)\n"),
          text("  (timestamp verified)\n"),
          text("  (type explicit))")
        ])
      ])

  | SubscribeConfirmed =>
      div([], [
        p([class'("success")], [text("✓ Consent cryptographically proven")]),
        code([], [text("proof : confirmation > initial_request")])
      ])

  | VerifyRequested =>
      div([], [
        p([], [text("Verifying consent chain...")])
      ])

  | VerificationShown =>
      div([], [
        p([], [text("✓ Verification complete")]),
        pre([class'("k9-svc-check")], [
          text("(k9-svc validate\n"),
          text("  (component consent-chain)\n"),
          text("  (invariant monotonic)\n"),
          text("  (status valid))")
        ])
      ])

  | UnsubscribeRequested =>
      div([], [
        p([], [text("Testing unsubscribe link...")]),
        pre([class'("k9-svc-check")], [
          text("(k9-svc validate\n"),
          text("  (link unsubscribe)\n"),
          text("  (response-code 200)\n"),
          text("  (response-time < 200ms))")
        ])
      ])

  | UnsubscribeConfirmed =>
      div([], [
        p([class'("success")], [text("✓ Unsubscribe proven working")]),
        p([], [text("Link was tested before sending!")])
      ])
  }

  div([id("tea-demo")], [
    section([class'("interactive-demo")], [
      h2([], [text("Interactive Demo (ReScript-TEA)")]),
      stepContent,
      p([class'("debug")], [
        text("Clicks: " ++ Int.toString(model.demoClicks))
      ])
    ])
  ])
}

let subscriptions = (_model: model) => Tea.Sub.none

let main = () => {
  Tea.App.standardProgram({
    init: init,
    update: update,
    view: view,
    subscriptions: subscriptions,
  })
}

// Initialize on load
@val external document: 'a = "document"
@send external addEventListener: ('a, string, unit => unit) => unit = "addEventListener"

document->addEventListener("DOMContentLoaded", () => main())
