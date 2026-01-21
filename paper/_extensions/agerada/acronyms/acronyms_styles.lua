--[[

    This file defines the "styles" to replace acronyms.

    Such styles control how to use the acronym's short name,
    long name, whether one should be between parentheses, etc.

    Styles are largely inspired from the LaTeX package "glossaries"
    (and "glossaries-extra").
    A gallery of the their styles can be found at:
    https://www.dickimaw-books.com/gallery/index.php?label=sample-abbr-styles
    A more complete document (rather long) can be found at:
    https://mirrors.chevalier.io/CTAN/macros/latex/contrib/glossaries-extra/samples/sample-abbr-styles.pdf

    More specifically, this file defines a table of functions.
    Each function takes an acronym, and return one or several Pandoc elements.
    These elements will replace the original acronym call in the Markdown 
    document.

    Most styles will depend on whether this is the acronym's first occurrence,
    ("first use") or not ("next use"), similarly to the LaTeX "glossaries".

    For example, a simple (default) style can be to return the acronym's
    long name, followed by the short name between parentheses.
    When the parser encounters `\acr{RL}`, assuming that `RL` is correctly
    defined in the acronyms database, the corresponding function would 
    return a Pandoc Link, where the text is "Reinforcement Learning (RL)",
    and pointing to the definition of "RL" in the List of Acronyms.
    
    Note: the acronym's key MUST exist in the acronyms database.
    Functions to replace a non-existing key must be handled elsewhere.

--]]

local Helpers = require("acronyms_helpers")


local function capitalize_first(s)
  return (s:gsub("^%l", string.upper))
end

-- The table containing all styles, indexed by the style's name.
local styles = {}


-- Local helper function to create either a Str or a Link,
-- depending on whether we want to insert links.
-- Use helpers from Helpers for inlines and rich element creation

-- Legacy create_element (unchanged original behavior): takes raw string content
-- and returns either a Link (with the content as inlines) or a Str.
local function create_element(content, key, insert_links)
    if insert_links then
        return pandoc.Link(content, Helpers.key_to_link(key))
    else
        return pandoc.Str(content)
    end
end


-- First use: long name (short name)
-- Next use: short name
styles["long-short"] = function(acronym, insert_links, is_first_use)
    if is_first_use then
    local longname_elem = Helpers.ensure_inlines(acronym.longname)
    local shortname_elem = Helpers.ensure_inlines(acronym.shortname)
        local all = {}
        for _, v in ipairs(longname_elem) do table.insert(all, v) end
        table.insert(all, pandoc.Str(" ("))
        for _, v in ipairs(shortname_elem) do table.insert(all, v) end
        table.insert(all, pandoc.Str(")"))
        if insert_links then
            return { pandoc.Link(all, Helpers.key_to_link(acronym.key)) }
        else
            return all
        end
    else
    local elem = Helpers.create_rich_element(acronym.shortname, acronym.key, insert_links)
        if type(elem) == "table" then return elem else return {elem} end
    end
end


-- First use: short name (long name)
-- Next use: short name
styles["short-long"] = function(acronym, insert_links, is_first_use)
    if is_first_use then
    local shortname_elem = Helpers.ensure_inlines(acronym.shortname)
        local longname_elem = Helpers.ensure_inlines(acronym.longname)
        local all = {}
        for _, v in ipairs(shortname_elem) do table.insert(all, v) end
        table.insert(all, pandoc.Str(" ("))
        for _, v in ipairs(longname_elem) do table.insert(all, v) end
        table.insert(all, pandoc.Str(")"))
        if insert_links then
            return { pandoc.Link(all, Helpers.key_to_link(acronym.key)) }
        else
            return all
        end
    else
    local elem = Helpers.create_rich_element(acronym.shortname, acronym.key, insert_links)
        if type(elem) == "table" then return elem else return {elem} end
    end
end

-- First use: long name
-- Next use: long name
styles["long-long"] = function(acronym, insert_links)
    local elem = Helpers.create_rich_element(acronym.longname, acronym.key, insert_links)
    if type(elem) == "table" then return elem else return {elem} end
end

-- First use: short name [^1]
-- [^1]: short name: long name
-- Next use: short name
styles["short-footnote"] = function(acronym, insert_links, is_first_use)
    if is_first_use then
        -- Main text: plain shortname (no link)
        local text = pandoc.Str(acronym.shortname)
        -- Footnote: [shortname](link): longname
    local shortname_link = create_element(acronym.shortname, acronym.key, insert_links)
    local longname_elem = Helpers.ensure_inlines(acronym.longname)
        local plain = {}
        if type(shortname_link) == "table" then for _, v in ipairs(shortname_link) do table.insert(plain, v) end else table.insert(plain, shortname_link) end
        table.insert(plain, pandoc.Str(": "))
        for _, v in ipairs(longname_elem) do table.insert(plain, v) end
        local note = pandoc.Note(pandoc.Plain(plain))
        return { text, note }
    else
    local elem = Helpers.create_rich_element(acronym.shortname, acronym.key, insert_links)
        if type(elem) == "table" then return elem else return {elem} end
    end
end


-- The "public" API of this module, the function which is returned by
-- require.
return function(acronym, style_name, insert_links, is_first_use, plural, 
    case_target, case)
    -- Check that the requested strategy exists
    assert(style_name ~= nil,
        "[acronyms] The parameter style_name must not be nil!")
    assert(styles[style_name] ~= nil,
        "[acronyms] Style " .. tostring(style_name) .. " does not exist!")

    -- Check that the acronym exists
    assert(acronym ~= nil,
        "[acronyms] The acronym must not be nil!")

    -- Determine if it is the first use (if left unspecified)
    if is_first_use == nil then
        is_first_use = acronym:isFirstUse()
    end

    -- Transform this acronym prior to rendering
    -- e.g., for plural form; and for sentence case
    acronym = acronym:clone()
    if plural then
        -- Conditional strictness: if markdown parsing is enabled for a part, require an explicit plural for that part.
        local need_long_strict = acronym._parse_markdown_longname and not acronym._explicit_plural_longname
        local need_short_strict = acronym._parse_markdown_shortname and not acronym._explicit_plural_shortname
        if need_long_strict then
            quarto.log.error("[acronyms] Plural form requested for '" .. tostring(acronym.key) .. "' but 'plural.longname' was not explicitly provided while markdown parsing is enabled for its longname. Define it under plural: { longname: ... } to use \\acrs{" .. tostring(acronym.key) .. "} .")
            assert(false)
        end
        if need_short_strict then
            quarto.log.error("[acronyms] Plural form requested for '" .. tostring(acronym.key) .. "' but 'plural.shortname' was not explicitly provided while markdown parsing is enabled for its shortname. Define it under plural: { shortname: ... } to use \\acrs{" .. tostring(acronym.key) .. "} .")
            assert(false)
        end
        -- Apply plural forms (explicit provided parts already present; fallbacks safe for non-markdown components).
        acronym.shortname = acronym.plural.shortname
        acronym.longname = acronym.plural.longname
    end

    -- Delegate case transformation to Helpers.transform_case which preserves inline formatting
    local function transform_case(value, case_kind)
        return Helpers.transform_case(value, case_kind)
    end

    if case == "upper" or case == "lower" or case == "sentence" then
        if case_target == "short" or case_target == "both" then
            acronym.shortname = transform_case(acronym.shortname, case)
        end
        if case_target == "long" or case_target == "both" then
            acronym.longname = transform_case(acronym.longname, case)
        end
    end

    local rendered = styles[style_name](acronym, insert_links, is_first_use, case_target)
    return rendered
end
