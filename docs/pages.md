---
layout: page
title: Pages
header: Pages
maint: true
redirect_to: /archive
---
{% include JB/setup %}

<h2>All Pages</h2>
<ul>
{%- assign pages_list = site.pages
    | where_exp: 'item', 'item.redirect_to == nil'
    | where_exp: 'item', 'item.maint == nil'
    | sort: 'title' -%}
{%- include JB/pages_list %}
</ul>

<!--
This file has been left for redirection.
Please do not add any content.
Redirect to /packages/ac-ispell.
This file will be deleted after 6 month (2020/03/01).

;; Local Variables:
;; buffer-read-only: t
;; End:
-->
