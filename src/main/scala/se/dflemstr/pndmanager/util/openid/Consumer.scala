package se.dflemstr.pndmanager.util.openid

import _root_.org.openid4java.message.ax.FetchRequest
import _root_.org.openid4java.message.sreg.SRegRequest
import _root_.net.liftweb.openid._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._

class Consumer[T] extends OpenIdConsumer[T] {
  override def authRequest(userSuppliedString: String, targetUrl: String): LiftResponse = {
    val returnToUrl = S.encodeURL(S.hostAndPath + targetUrl)

    Log.info("Creating openId auth request. Return to url: " + returnToUrl)

    val discoveries = manager.discover(userSuppliedString)
    val discovered = manager.associate(discoveries)

    S.servletSession.foreach(_.setAttribute("openid-disc", discovered))

    val authReq = manager.authenticate(discovered, returnToUrl)

    val sregReq = SRegRequest.createFetchRequest()
    sregReq.addAttribute("email", true)
    sregReq.addAttribute("nickname", true)
    sregReq.addAttribute("language", true)
    sregReq.addAttribute("timezone", true)

    authReq.addExtension(sregReq)

    val fetch = FetchRequest.createFetchRequest()
    fetch.addAttribute("email", "http://axschema.org/contact/email", true)
    fetch.addAttribute("nickname", "http://axschema.org/namePerson/friendly", true)
    fetch.addAttribute("language", "http://axschema.org/pref/language", true)
    fetch.addAttribute("timezone", "http://axschema.org/pref/timezone", true)
    
    authReq.addExtension(fetch)

    if(!discovered.isVersion2()) {
      // Option 1: GET HTTP-redirect to the OpenID Provider endpoint
      // The only method supported in OpenID 1.x
      // redirect-URL usually limited ~2048 bytes
      RedirectResponse(authReq.getDestinationUrl(true))
    } else {
      // Option 2: HTML FORM Redirection (Allows payloads >2048 bytes)
      val pm =  authReq.getParameterMap()
      val info: Seq[(String, String)] = pm.keySet.toArray.
        map(k => (k.toString, pm.get(k).toString))

      XhtmlResponse(
        <html xmlns="http://www.w3.org/1999/xhtml">
          <head>
            <title>OpenID HTML FORM Redirection</title>
          </head>
          <body onload="document.forms['openid-form-redirection'].submit();">
            <form name="openid-form-redirection" action={authReq.getDestinationUrl(false)} method="post" accept-charset="utf-8">
              {
                info.map{ case(key, value) =>
                    <input type="hidden" name={key} value={value}/>
                }
              }
              <p>Now redirecting you to your OpenID provider...</p>
              <button type="submit">Continue manually</button>
            </form>
          </body>
        </html>, Empty, Nil, Nil, 200, true)
    }
  }
}
