## Dockerfile

FROM ubuntu:bionic

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt -y upgrade

# In Gemfile:
#   github-pages was resolved to 206, which depends on
#     jekyll-commonmark-ghpages was resolved to 0.1.6, which depends on
#       jekyll-commonmark was resolved to 1.3.1, which depends on
#         commonmarker

# In Gemfile:
#   github-pages was resolved to 206, which depends on
#     jekyll-mentions was resolved to 1.5.1, which depends on
#       html-pipeline was resolved to 2.13.0, which depends on
#         nokogiri

# commonmarker requires make
# nokogiri requires zlib
RUN apt install -y build-essential ruby ruby-dev zlib1g-dev
RUN gem update --system
RUN gem install bundler

COPY Gemfile /

RUN bundle install
