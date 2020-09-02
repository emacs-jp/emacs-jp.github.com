---
layout: page
title: Categories
header: Posts By Category
maint: true
redirect_to: /tags
---
{% include JB/setup %}

<ul class="tag_box inline">
  {% assign categories_list = site.categories %}
  {% include JB/categories_list %}
</ul>


{% for category in site.categories %} 
  <h2 id="{{ category[0] }}-ref">{{ category[0] | join: "/" }}</h2>
  <ul>
    {% assign pages_list = category[1] %}  
    {% include JB/pages_list %}
  </ul>
{% endfor %}

<!--
This file has been left for redirection.
Please do not add any content.
Redirect to /tags.
This file will be deleted after 6 month (2020/03/01).

;; Local Variables:
;; buffer-read-only: t
;; End:
-->
