Span = function(sp)
  if sp.classes:includes("defword") then
    --print(sp)
    if sp.attributes and sp.attributes.title then
      local unprocessedtitle = quarto.utils.string_to_inlines(sp.attributes.title)
      local sptitle = unprocessedtitle
      sptitle = sptitle:walk {
        Strong = function (e) return pandoc.Str("<strong>" .. pandoc.utils.stringify(e.content) .. "</strong>") end,
        Emph = function (e) return pandoc.Str("<em>" .. pandoc.utils.stringify(e.content) .. "</em>") end,
        Subscript = function (e) return pandoc.Str("<sub>" .. pandoc.utils.stringify(e.content) .. "</sub>") end,
        Superscript = function (e) return pandoc.Str("<sup>" .. pandoc.utils.stringify(e.content) .. "</sup>") end
        }

      sp.attributes.title = pandoc.utils.stringify(sptitle)
      local sp_definition = pandoc.Span(unprocessedtitle, {class = "defword column-margin"})
      sp.attributes["data-bs-html"] = "true"
      sp.attributes["data-bs-toggle"] = "tooltip"
      if sp.attributes.inmargin and sp.attributes.inmargin == "false" then
        return sp
      else
        return pandoc.Inlines({sp, sp_definition})
      end
    end
  end
end
