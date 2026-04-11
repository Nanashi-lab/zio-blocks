package zio.blocks.htmx

import zio.blocks.template.Dom

trait Attributes {
  val hxGet: HtmxAttribute            = new HtmxAttribute("hx-get")
  val hxPost: HtmxAttribute           = new HtmxAttribute("hx-post")
  val hxPut: HtmxAttribute            = new HtmxAttribute("hx-put")
  val hxPatch: HtmxAttribute          = new HtmxAttribute("hx-patch")
  val hxDelete: HtmxAttribute         = new HtmxAttribute("hx-delete")
  val hxConfirm: HtmxAttribute        = new HtmxAttribute("hx-confirm")
  val hxPrompt: HtmxAttribute         = new HtmxAttribute("hx-prompt")
  val hxSwap: HtmxAttribute           = new HtmxAttribute("hx-swap")
  val hxTrigger: HtmxAttribute        = new HtmxAttribute("hx-trigger")
  val hxTarget: HtmxAttribute         = new HtmxAttribute("hx-target")
  val hxSelect: HtmxAttribute         = new HtmxAttribute("hx-select")
  val hxSelectOob: HtmxAttribute      = new HtmxAttribute("hx-select-oob")
  val hxIndicator: HtmxAttribute      = new HtmxAttribute("hx-indicator")
  val hxDisabledElt: HtmxAttribute    = new HtmxAttribute("hx-disabled-elt")
  val hxInclude: HtmxAttribute        = new HtmxAttribute("hx-include")
  val hxVals: HtmxAttribute           = new HtmxAttribute("hx-vals")
  val hxHeaders: HtmxAttribute        = new HtmxAttribute("hx-headers")
  val hxParams: HtmxAttribute         = new HtmxAttribute("hx-params")
  val hxRequest: HtmxAttribute        = new HtmxAttribute("hx-request")
  val hxSync: HtmxAttribute           = new HtmxAttribute("hx-sync")
  val hxEncoding: HtmxAttribute       = new HtmxAttribute("hx-encoding")
  val hxExt: HtmxAttribute            = new HtmxAttribute("hx-ext")
  val hxDisinherit: HtmxAttribute     = new HtmxAttribute("hx-disinherit")
  val hxInherit: HtmxAttribute        = new HtmxAttribute("hx-inherit")
  val hxSwapOob: HtmxSwapOobAttribute = new HtmxSwapOobAttribute("hx-swap-oob")

  val hxPushUrl: HtmxUrlOrBooleanAttribute    = new HtmxUrlOrBooleanAttribute("hx-push-url")
  val hxReplaceUrl: HtmxUrlOrBooleanAttribute = new HtmxUrlOrBooleanAttribute("hx-replace-url")

  private[this] val hxBoostAttribute      = new HtmxBooleanStringAttribute("hx-boost")
  private[this] val hxDisableAttribute    = new HtmxPresenceAttribute("hx-disable")
  private[this] val hxHistoryEltAttribute = new HtmxPresenceAttribute("hx-history-elt")
  private[this] val hxHistoryAttribute    = new HtmxFalseOnlyAttribute("hx-history")
  private[this] val hxPreserveAttribute   = new HtmxStringTrueAttribute("hx-preserve")
  private[this] val hxValidateAttribute   = new HtmxBooleanStringAttribute("hx-validate")

  def hxBoost: Dom.Attribute =
    hxBoostAttribute()

  def hxBoost(enabled: Boolean): Dom.Attribute =
    hxBoostAttribute := enabled

  def hxDisable: Dom.Attribute =
    hxDisableAttribute()

  def hxHistory(enabled: Boolean): Dom.Attribute =
    hxHistoryAttribute := enabled

  def hxHistoryElt: Dom.Attribute =
    hxHistoryEltAttribute()

  def hxPreserve: Dom.Attribute =
    HtmxValue.presence.render("hx-preserve", ())

  def hxPreserve(enabled: Boolean): Dom.Attribute =
    hxPreserveAttribute := enabled

  def hxValidate: Dom.Attribute =
    hxValidateAttribute()

  def hxValidate(enabled: Boolean): Dom.Attribute =
    hxValidateAttribute := enabled

  val hxOn: HxOn = HxOn
}
