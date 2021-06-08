## Dockerfile

FROM ruby:2.7

COPY Gemfile /

RUN bundle install
