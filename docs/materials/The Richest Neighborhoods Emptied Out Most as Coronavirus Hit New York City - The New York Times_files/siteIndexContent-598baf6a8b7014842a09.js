(window.webpackJsonp=window.webpackJsonp||[]).push([[82],{KFDm:function(e,i,a){"use strict";a.r(i);var n=a("gcR/"),t=a.n(n),o=a("q1tI"),c=a.n(o),s=(a("17x9"),a("Jqp9")),l=a("X6oL"),d=a("i9j7"),r=a.n(d),p=a("wXC7"),m=a.n(p),u=a("KeRS"),h=a.n(u),f=(a("+2oP"),a("2B1R"),a("ma9I"),a("DQNa"),a("07d7"),a("JfAA"),a("sMBO"),a("mRH6"),a("J4zp")),b=a.n(f);a("QWBl"),a("FZtP"),a("yXV3"),a("pDQq");function v(e){var i=e.className,a=e.children,n=e.uniqueName,s=e.alwaysOpen,l=e.allowMultiple,d=[];o.Children.forEach(a,(function(e,i){e.props.expanded&&d.push(i)}));var r=Object(o.useState)(d),p=b()(r,2),m=p[0],u=p[1];return t()("div",{className:i,role:"tablist","aria-multiselectable":"true","data-testid":"accordion"},void 0,o.Children.map(a,(function(e,i){return c.a.cloneElement(e,{expanded:-1!==m.indexOf(i),index:i,uid:"".concat(n,"-").concat(i),onClick:function(){return function(e,i){"function"==typeof i&&i();var a=m.slice(0),n=1===a.length&&s,t=a.indexOf(e);-1===t||n?l?a.push(e):a=[e]:a.splice(t,1),u(a)}(i,e.props.onClick)}})})))}v.displayName="Accordion",v.defaultProps={allowMultiple:!1,alwaysOpen:!1,className:" ",children:null};var g=v,x=a("lSNA"),y=a.n(x);function k(e){var i=e.uid,a=e.onClick,n=e.className,l=e.expanded,d=e.expandedClassName,r=e.children;return t()("div",{className:Object(s.cx)(n,y()({},d,l)),"data-testid":"accordion-item"},void 0,o.Children.map(r,(function(e){return c.a.cloneElement(e,{onClick:a,expanded:l,uid:i})})))}k.displayName="AccordionItem",k.defaultProps={onClick:function(){},expanded:!1,expandedClassName:void 0,className:void 0,uid:"",children:null};var w=k;function N(e){var i=e.uid,a=e.expanded,n=e.className,o=e.children,c={};return a||(c.display="none"),t()("div",{className:Object(s.cx)(Object(s.css)(c),n),id:"body-".concat(i),"aria-labelledby":"item-".concat(i),role:"tabpanel","data-testid":"accordion-item-body"},void 0,o)}N.displayName="AccordionItemBody",N.defaultProps={expanded:!1,className:void 0,uid:"",children:null};var C=N,O=Object(s.css)("cursor:pointer;margin:0;");function j(e){var i=e.uid,a=e.className,n=e.onClick,o=e.expanded,c=e.children;return t()("header",{"aria-controls":"body-".concat(i),id:"item-".concat(i),className:Object(s.cx)(O,a),onClick:n,onKeyUp:function(e){return!n||13!==e.keyCode&&32!==e.keyCode||(n(),!1)},role:"tab",tabIndex:"0","aria-expanded":o,"data-testid":"accordion-item-header"},void 0,c)}j.displayName="AccordionItemHeader",j.defaultProps={expanded:!1,className:void 0,children:null,uid:"",onClick:null};a("zHFu");var D=Object(s.css)("list-style:none;margin:0;padding:0;"),H=(m.a.color.gray60,m.a.breakpoint.medium,m.a.color.gray60,m.a.color.gray60,m.a.breakpoint.large,m.a.breakpoint.maxDesktopContentWidth,Object(s.css)("padding:0 20px;",m.a.breakpoint.medium,"{padding:0 3%;}",m.a.breakpoint.large,"{padding:0;}")),S=Object(s.css)("display:flex;flex-flow:row;"),L=Object(s.css)("display:block;height:44px;vertical-align:middle;width:184px;"),M=Object(s.css)("margin:18px 0 0 auto;"),T=Object(s.css)("color:",m.a.color.blue30,";font-family:",m.a.font.franklinBase,";font-size:11px;font-style:normal;font-weight:",m.a.font.weight.book,";line-height:11px;text-decoration:none;"),z=Object(s.css)("display:block;",m.a.breakpoint.large,"{display:none;}"),I=Object(s.css)("display:none;",m.a.breakpoint.large,"{display:block;}"),P=Object(s.css)("display:flex;margin-top:10px;min-width:600px;"),A=Object(s.css)("flex:1;"),V=Object(s.css)("border-left:1px solid ",m.a.color.gray60,";flex:1;padding-left:15px;"),B=Object(s.css)("color:",m.a.color.gray20,";font-size:13px;font-weight:",m.a.font.weight.bold,";font-family:",m.a.font.franklinBase,";height:25px;line-height:15px;margin:0;text-transform:uppercase;width:150px;"),E=Object(s.css)("margin-bottom:5px;white-space:nowrap;&:last-child{margin-bottom:10px;}"),q=Object(s.css)("color:",m.a.color.black,";display:inline-block;font-family:",m.a.font.franklinBase,";text-decoration:none;text-transform:capitalize;width:150px;&:hover{cursor:pointer;text-decoration:underline;}body.dark &{color:",m.a.color.white,";}"),R=Object(s.css)("&.desktop{display:none;}",m.a.breakpoint.medium,"{&.desktop{display:block;}&.smartphone{display:none;}}"),F=Object(s.css)("border-top:1px solid ",m.a.color.gray70,";color:",m.a.color.gray20,";font-family:",m.a.font.franklinBase,";font-size:13px;font-weight:",m.a.font.weight.bold,";height:44px;letter-spacing:0.04rem;line-height:44px;text-transform:uppercase;.accordionExpanded &{color:",m.a.color.gray45,";}"),J=Object(s.css)(D,";columns:2;padding:0 0 15px;"),G=Object(s.css)("height:34px;line-height:34px;list-style-type:none;&.desktop{display:none;}",m.a.breakpoint.medium,"{&.desktop{display:block;}&.smartphone{display:none;}}"),Q=Object(s.css)("color:",m.a.color.gray20,";display:block;font-family:",m.a.font.franklinBase,";font-size:15px;font-weight:",m.a.font.weight.medium,";height:34px;line-height:34px;text-decoration:none;text-transform:capitalize;"),K=Object(s.css)(q,";font-size:14px;font-weight:",m.a.font.weight.medium,";height:23px;line-height:16px;"),W=Object(s.css)(q,";font-size:16px;font-weight:",m.a.font.weight.bold,";height:25px;line-height:15px;padding-bottom:0;"),X=Object(s.css)(q,";font-size:11px;font-weight:",m.a.font.weight.medium,";height:23px;line-height:21px;"),Y=Object(s.css)(D,";border-top:1px solid ",m.a.color.gray60,";margin-top:2px;padding-top:10px;"),U=Object(s.css)("display:inline-block;height:13px;width:13px;margin-right:7px;vertical-align:middle;"),Z=Object(s.css)(U,";"),$=Object(s.css)(U,";"),_=Object(s.css)(U,";"),ee=Object(s.css)(U,";"),ie="Listings & More";function ae(e){var i=e.indexData,a=e.uniqueName,n=e.handleAccordionClick,o=function(e){if(n){var i,t,o=(t=i=a,"masthead"===i?t="header":"siteindex"===i&&(t="footer"),t),c=function(e){return"more"===e?ie:e.charAt(0).toUpperCase()+e.slice(1)}(e);n(o,c)}};return t()(g,{uniqueName:a},void 0,i.map((function(e,i){return t()(w,{expandedClassName:"accordionExpanded",expanded:e.expanded,onClick:function(){return o(e.name)}},"".concat(a,"-").concat(i.toString(16)),t()(j,{className:F,index:i},void 0,e.longName||e.name),t()(C,{},void 0,t()("ul",{className:J,"data-testid":"site-index-accordion-list"},void 0,e.pages.map((function(e){return t()("li",{className:Object(s.cx)(G,e.deviceType)},e.link,t()("a",{className:Q,href:e.link,"data-testid":"accordion-item-list-link"},void 0,e.name))})))))})))}ae.displayName="SiteIndexAccordion",ae.defaultProps={handleAccordionClick:null,indexData:[]};var ne,te,oe,ce,se,le,de,re,pe,me,ue=ae,he=(a("yq1k"),a("JTJg"),a("fbCW"),a("PgO9")),fe=(a("yyme"),function(e){var i=e.className,a=e.fill;return t()("svg",{className:i,viewBox:"0 0 13 13",fill:a},void 0,ne||(ne=t()("polygon",{points:"0,-93.6 0,-86.9 6.6,-93.6"})),te||(te=t()("polygon",{points:"0.9,-86 7.5,-86 7.5,-92.6"})),oe||(oe=t()("polygon",{points:"0,-98 0,-94.8 8.8,-94.8 8.8,-86 12,-86 12,-98"})),ce||(ce=t()("path",{d:"M11.9-40c-0.4,1.1-1.2,1.9-2.3,2.4V-40l1.3-1.2l-1.3-1.2V-44c1.2-0.1,2-1,2-2c0-1.4-1.3-1.9-2.1-1.9c-0.2,0-0.3,0-0.6,0.1v0.1c0.1,0,0.2,0,0.3,0c0.5,0,0.9,0.2,0.9,0.7c0,0.4-0.3,0.7-0.8,0.7c-1.3,0-2.8-1.1-4.5-1.1c-1.5,0-2.5,1.1-2.5,2.2c0,1.1,0.6,1.5,1.3,1.7l0-0.1c-0.2-0.1-0.4-0.4-0.4-0.7c0-0.5,0.5-0.9,1-0.9C5.7-45.1,8-44,9.4-44h0.1v1.7l-1.3,1.1L9.5-40v2.4c-0.5,0.2-1.1,0.3-1.7,0.3c-2.2,0-3.6-1.3-3.6-3.5c0-0.5,0.1-1,0.2-1.5l1.1-0.5v4.9l2.2-1v-5l-3.3,1.5c0.3-1,1-1.7,1.8-2l0,0c-2.2,0.5-4.3,2.1-4.3,4.6c0,2.9,2.4,4.8,5.2,4.8C10.2-35.1,11.9-37.1,11.9-40L11.9-40z"})),se||(se=t()("path",{d:"M12.2-23.7c-0.2,0-0.4,0.2-0.4,0.4v0.4L0.4-19.1v2.3l3,1l-0.2,0.6c-0.3,0.8,0.1,1.8,0.9,2.1l1.7,0.7c0.2,0.1,0.4,0.1,0.6,0.1c0.6,0,1.3-0.4,1.5-1l0.4-0.9l3.5,1.2v0.4c0,0.2,0.2,0.4,0.4,0.4c0.2,0,0.4-0.2,0.4-0.4v-10.7C12.6-23.5,12.4-23.7,12.2-23.7M7.1-13.6c-0.2,0.4-0.6,0.6-1,0.4l-1.7-0.7c-0.4-0.2-0.6-0.6-0.4-1l0.3-0.7l3.3,1.1L7.1-13.6z"})),le||(le=t()("path",{d:"M13.1-60.3H3.5v-10.5h9.6V-60.3zM13.1-71.6H3.5c-0.5,0-0.9,0.4-0.9,0.9v2.2H0.9c-0.5,0-0.9,0.4-0.9,0.9v5.2v1.5c0,0.8,0.8,1.5,1.8,1.5h1.7h0h7.4h2.2c0.5,0,0.9-0.4,0.9-0.9v-10.5C14-71.2,13.6-71.6,13.1-71.6"})),de||(de=t()("polygon",{points:"10.9,-69 5.2,-69 5.2,-68.1 11.4,-68.1 11.4,-69"})),re||(re=t()("rect",{x:"5.2",y:"-67.3",width:"6.1",height:"0.9"})),pe||(pe=t()("rect",{x:"5.2",y:"-65.5",width:"6.1",height:"0.9"})),me||(me=t()("path",{d:"M12,6.5H6.5V12H1V6.5h5.5V1H12V6.5zM12,0H1C0.4,0,0,0.5,0,1v11c0,0.6,0.4,1,1,1h11c0.5,0,1-0.4,1-1V1C13,0.5,12.5,0,12,0"})))});fe.displayName="CrosswordIcon",fe.defaultProps={className:void 0,fill:m.a.color.black};var be=fe,ve=function(e){var i=e.className,a=e.fill;return t()("svg",{className:i,viewBox:"0 0 10 13"},void 0,t()("path",{fill:a,d:"M9.9,8c-0.4,1.1-1.2,1.9-2.3,2.4V8l1.3-1.2L7.6,5.7V4c1.2-0.1,2-1,2-2c0-1.4-1.3-1.9-2.1-1.9c-0.2,0-0.3,0-0.6,0.1v0.1c0.1,0,0.2,0,0.3,0c0.5,0,0.9,0.2,0.9,0.7c0,0.4-0.3,0.7-0.8,0.7C6,1.7,4.5,0.6,2.8,0.6c-1.5,0-2.5,1.1-2.5,2.2C0.3,4,1,4.3,1.6,4.6l0-0.1C1.4,4.4,1.3,4.1,1.3,3.8c0-0.5,0.5-0.9,1-0.9C3.7,2.9,6,4,7.4,4h0.1v1.7L6.2,6.8L7.5,8v2.4c-0.5,0.2-1.1,0.3-1.7,0.3c-2.2,0-3.6-1.3-3.6-3.5c0-0.5,0.1-1,0.2-1.5l1.1-0.5V10l2.2-1v-5L2.5,5.5c0.3-1,1-1.7,1.8-2l0,0C2.2,3.9,0.1,5.6,0.1,8c0,2.9,2.4,4.8,5.2,4.8C8.2,12.9,9.9,10.9,9.9,8L9.9,8z"}))};ve.displayName="DigitalSubscriptionsIcon",ve.defaultProps={className:void 0,fill:m.a.color.black};var ge,xe,ye,ke,we=ve,Ne=function(e){var i=e.className,a=e.fill;return t()("svg",{className:i,viewBox:"0 0 14 13",fill:a},void 0,ge||(ge=t()("path",{d:"M13.1,11.7H3.5V1.2h9.6V11.7zM13.1,0.4H3.5C3,0.4,2.6,0.8,2.6,1.2v2.2H0.9C0.4,3.4,0,3.8,0,4.3v5.2v1.5c0,0.8,0.8,1.5,1.8,1.5h1.7h0h7.4h2.2c0.5,0,0.9-0.4,0.9-0.9V1.2C14,0.8,13.6,0.4,13.1,0.4"})),xe||(xe=t()("polygon",{points:"10.9,3 5.2,3 5.2,3.9 11.4,3.9 11.4,3"})),ye||(ye=t()("rect",{x:"5.2",y:"4.7",width:"6.1",height:"0.9"})),ke||(ke=t()("rect",{x:"5.2",y:"6.5",width:"6.1",height:"0.9"})))};Ne.displayName="HomeDeliveryIcon",Ne.defaultProps={className:void 0,fill:m.a.color.black};var Ce,Oe,je=Ne,De=function(e){var i=e.className,a=e.fill;return t()("svg",{className:i,viewBox:"0 0 13 13",fill:a},void 0,Ce||(Ce=t()("path",{d:"M12,2.9L9.6,5.2c-0.1,0.1-0.3,0.1-0.4,0C9.1,5.2,9.1,5,9.3,4.9l2.4-2.4c-0.2-0.2-0.3-0.3-0.5-0.5L8.7,4.3c-0.1,0.1-0.3,0.1-0.4,0C8.2,4.3,8.2,4.1,8.4,4l2.4-2.4c-0.3-0.3-0.5-0.5-0.5-0.5L7.6,3.4C7.1,4,6.8,5.1,7.1,5.8c-1.4,1-4.6,3.5-5.1,4c-0.8,0.8-0.4,1.8-0.3,1.9c0,0,0,0,0,0c0,0,0,0,0,0c0.1,0.1,1.1,0.5,1.9-0.3c0.4-0.4,2.9-3.6,3.9-5C8.4,6.9,9.6,6.6,10.2,6l2.3-2.6C12.5,3.4,12.3,3.2,12,2.9z"})),Oe||(Oe=t()("path",{d:"M0.8,1.9l0.3-0.3c0.9-0.9,3.2,1.1,3.8,1.7s0.9,1.8,0.4,2.6c1.4,1.1,4.6,3.5,5,3.9c0.8,0.8,0.4,1.8,0.3,1.9c0,0,0,0,0,0c0,0,0,0,0,0c-0.1,0.1-1.1,0.5-1.9-0.3c-0.4-0.4-2.9-3.7-4-5.1C3.9,6.7,2.9,6.4,2.3,5.8S-0.2,2.9,0.8,1.9z"})))};De.displayName="CookingIcon",De.defaultProps={className:void 0,fill:m.a.color.black};var He=De,Se=a("GB3f"),Le={productLinks:[{name:"Newsletters",link:"https://www.nytimes.com/newsletters",iconClass:"iconHomeDelivery",userType:"hd"},{name:"Home Delivery",link:"https://www.nytimes.com/hdleftnav",iconClass:"iconHomeDelivery",userType:["nonsub","digital"]},{name:"Digital Subscriptions",link:"https://www.nytimes.com/digitalleftnav",iconClass:"iconDigitalSubscriptions",userType:"nonsub"},{name:"Gift Subscriptions",link:"https://www.nytimes.com/gift",iconClass:"iconDigitalSubscriptions",userType:["digital","hd"]},{name:"Games",link:"https://www.nytimes.com/subscription/games?campaignId=9LRLQ ",iconClass:"iconCrossword"},{name:"Cooking",link:"https://www.nytimes.com/subscription/cooking.html",iconClass:"iconCooking"}],corporateLinks:[{name:"Email Newsletters",link:"https://www.nytimes.com/newsletters",userType:["nonsub","digital"]},{name:"Corporate Subscriptions",link:"https://www.nytimes.com/corporateleftnav"},{name:"Education Rate",link:"https://www.nytimes.com/educationleftnav"}],alternateLinks:[{name:"Mobile Applications",link:"https://help.nytimes.com/hc/en-us/sections/115003859548-Apps"},{name:"Replica Edition",link:"http://eedition.nytimes.com/cgi-bin/signup.cgi?cc=37FYY"},{name:"International",link:"https://www.nytimes.com/international/?action=click&region=Footer&pgtype=Homepage",edition:"INT"},{name:"Canada",link:"https://www.nytimes.com/ca/?action=click&region=Footer&pgtype=Homepage",edition:"CA"},{name:"Español",link:"https://www.nytimes.com/es/?action=click&region=Footer&pgtype=Homepage",edition:"ES"},{name:"中文网",link:"https://cn.nytimes.com/"}]},Me={iconCrossword:be,iconDigitalSubscriptions:we,iconHomeDelivery:je,iconCooking:He},Te={iconCrossword:Z,iconDigitalSubscriptions:$,iconHomeDelivery:_,iconCooking:ee},ze="Subscribe",Ie=function(e){var i,a=e.subscribeData,n=Object(he.a)().user,o=Object(s.useTheme)(),c=a||Le,l="nonsub",d=null==n?void 0:n.entitlements,r=null==n||null===(i=n.demographics)||void 0===i?void 0:i.bundleSubscriptions;return d&&(d.includes("TPR")||d.includes("MTD")||d.includes("MSD")||d.includes("MM"))&&(l="digital"),null!=r&&r.find((function(e){return"H"===e.bundle}))&&(l="hd"),t()("div",{className:V,"aria-labelledby":"site-index-subscribe-label"},void 0,t()("h3",{className:B,id:"site-index-subscribe-label"},void 0,ze),t()("ul",{className:D,"data-testid":"site-index-subscribe-list"},void 0,c.productLinks.map((function(e){var i=null;if(e.iconClass){var a,n=Me[e.iconClass]||"i",c=Te[e.iconClass]||e.iconClass;Me[e.iconClass]&&"dark"===(null==o?void 0:o.mode)&&(a=m.a.color.white),i=t()(n,{className:c,fill:a})}return(!e.userType||e.userType.includes(l))&&t()("li",{className:E},e.name,t()("a",{className:W,href:e.link,"data-testid":"site-index-subscribe-list-link"},void 0,i,e.name))}))),t()("ul",{className:D,"data-testid":"site-index-corporate-links"},void 0,c.corporateLinks.map((function(e){return(!e.userType||e.userType.includes(l))&&t()("li",{},e.name,t()("a",{className:X,href:e.link},void 0,e.name))}))),t()("ul",{className:Y,"data-testid":"site-index-alternate-links"},void 0,c.alternateLinks.map((function(e){return(!e.userType||e.userType.includes(l))&&t()("li",{},e.name,t()("a",{className:X,href:e.link,onClick:function(){return(null==e?void 0:e.edition)&&Object(Se.d)(null==e?void 0:e.edition)}},void 0,e.name))}))))};Ie.displayName="SiteIndexSubscribe",Ie.defaultProps={subscribeData:void 0};var Pe,Ae=Ie,Ve=function(e){var i=e.indexData;return t()("div",{className:P,"data-testid":"site-index-section"},void 0,(i||[]).map((function(e,i){return t()("section",{className:A,"aria-labelledby":"site-index-section-label-".concat(i)},i.toString(),t()("h3",{className:B,id:"site-index-section-label-".concat(i)},void 0,e.name),t()("ul",{className:D,"data-testid":"site-index-section-list"},void 0,e.pages.map((function(e){var i=e.name,a=e.link,n=e.deviceType;return t()("li",{className:Object(s.cx)(R,n)},a,t()("a",{className:K,href:a,"data-testid":"site-index-section-list-link"},void 0,i))}))))})),Pe||(Pe=t()(Ae,{})))};Ve.displayName="SiteIndexSections",Ve.defaultProps={indexData:[]};var Be=Ve,Ee="Go to Home Page »";function qe(e){var i=e.indexData,a=Object(s.useTheme)(),n=Object(l.useTracking)();return t()("div",{className:H},void 0,t()("header",{className:S,"data-testid":"site-index-header"},void 0,t()(h.a,{to:"/","aria-label":"New York Times"},void 0,t()(r.a,{fill:"dark"===(null==a?void 0:a.mode)?m.a.color.white:m.a.color.black,className:L})),(!a||!a.homepage)&&t()("div",{className:M,"data-testid":"go-to-homepage"},void 0,t()(h.a,{to:"/",className:T},void 0,Ee))),t()("div",{className:z,"data-testid":"site-index-accordion"},void 0,t()(ue,{handleAccordionClick:function(e,i){n.trackEvent({event:"moduleInteraction",interaction:{module:{name:e,context:"section",label:i},type:"click",status:""}})},indexData:i,uniqueName:"siteindex"})),t()("div",{className:I,"data-testid":"site-index-sections"},void 0,t()(Be,{indexData:i})))}qe.displayName="SiteIndexContent",qe.defaultProps={indexData:[]};i.default=qe}}]);
//# sourceMappingURL=siteIndexContent-598baf6a8b7014842a09.js.map