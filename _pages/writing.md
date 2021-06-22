---
layout: archive
title: "Writing"
permalink: /writing/
author_profile: true
---

While my primary focus is on academic writing, you can find [a selection of my other published analysis here](https://zacharylhertz.github.io/otherwork/). Below, see my academic papers, with others forthcoming.

{% if author.googlescholar %}
  You can also find my articles on <u><a href="{{author.googlescholar}}">my Google Scholar profile</a>.</u>
{% endif %}

{% include base_path %}

{% for post in site.publications reversed %}
  {% include archive-single.html %}
{% endfor %}
