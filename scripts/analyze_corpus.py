import csv
import re
import os
from nltk.tokenize import wordpunct_tokenize
from liwc import Liwc

import logging

LOGGER = logging.getLogger()
LOGGER.setLevel(logging.DEBUG)
NON_PUNCT = re.compile('.*[A-Za-z0-9].*')
DICTIONARY = './data/dictionaries/{dictionary}.dic'
INPUT = './data/corpora/{corpus}/{file}'
OUTPUT_CORPUS = './data/computed/{corpus}'
OUTPUT_DICTIONARY = './data/computed/{corpus}/{dictionary}'
OUTPUT_FILE = './data/computed/{corpus}/{dictionary}/{file}.csv'


def _parse_categories(lines):
    """
    Read (category_id, category_name) pairs from the categories section.
    Each line consists of an integer followed a tab and then the category name.
    This section is separated from the lexicon by a line consisting of a single "%".
    """
    LOGGER.info("Parsing Categories")
    for line in lines:
        line = line.strip()
        if line == "%":
            return

        # ignore non-matching groups of categories
        if "\t" in line:
            category_id, category_name = line.split("\t", 1)
            yield category_id, category_name


def _parse_words(dictionary):
    with open(DICTIONARY.format(dictionary=dictionary)) as word_lines:
        # read up to first "%" (should be very first line of file)
        for line in word_lines:
            if line.strip() == "%":
                break

        # Ignore up to second "%"
        for line in word_lines:
            if line.strip() == "%":
                break

        for line in word_lines:
            line = line.strip()
            yield line.split('\t', 1)[0]


def dict_categories(dictionary):
    with open(DICTIONARY.format(dictionary=dictionary)) as lines:
        # read up to first "%" (should be very first line of file)
        for line in lines:
            if line.strip() == "%":
                break

        category_mapping = dict(_parse_categories(lines))

    return list(category_mapping.values())


def process_corpus(corpus, dictionary, files):
    LOGGER.addHandler(logging.FileHandler('./logs/{}_{}.log'.format(corpus, dictionary)))
    LOGGER.info('Starting Processing {} On Dictionary {}'.format(corpus, dictionary))

    # Get dictionary
    try:
        parser = Liwc(DICTIONARY.format(dictionary=dictionary))
    except Exception as error:
        LOGGER.info(error)
    
    category_names = dict_categories(dictionary)
    category_words = _parse_words(dictionary)

    categories = [ category + '_wc' for category in category_names] + category_names
    ngram_words = [ word for word in category_words if '_' in word ]

    # Outfiles
    outfiles = []

    # Corpus check
    if not os.path.exists(OUTPUT_CORPUS.format(corpus=corpus)):
        LOGGER.info('Creating Corpus Directory')
        os.makedirs(OUTPUT_CORPUS.format(corpus=corpus))

    # Dictionary Check
    if not os.path.exists(
        OUTPUT_DICTIONARY.format(corpus=corpus, dictionary=dictionary)
    ):
        LOGGER.info('Creating Dictionary Directory')
        os.makedirs(
            OUTPUT_DICTIONARY.format(corpus=corpus, dictionary=dictionary)
        )

    # Loop through files
    for file in files:
        LOGGER.debug('Processing file: {}'.format(file))

        # Read file
        file_data = open(
            INPUT.format(
                corpus=corpus,
                file=file
            ), 'rb'
        )

        # Commit Evaluated scores to output file
        with open(
            OUTPUT_FILE.format(
                corpus=corpus,
                dictionary=dictionary,
                file=file.split('.')[0]
            ), 'w'
        ) as csvfile:
            writer = csv.DictWriter(
                csvfile,
                fieldnames=[
                    'speech_id',
                    'speaker_id',
                    'date',
                    'unit',
                    'word_count',
                ] + categories
            )
            writer.writeheader()

            LOGGER.debug('File read. Processing rows')
            # Read in file row by row
            for i, row in enumerate(file_data):
                # Ignore Header Row
                if i == 0:
                    continue

                # Split row into columns
                try:
                    row_columns = row.decode().split('|')

                except Exception:
                    # Likely a decode error, less than 1000 total
                    LOGGER.debug('Decode Error -- Ignoring')
                    continue

                text = row_columns[4].lower()
                for ngram in ngram_words:
                    ngram_spaces = ngram.replace('_', ' ')
                    text.replace(ngram_spaces, ngram)

                ###
                # Run Word Count Analysis on speech
                ###
                # Build Speech scores dict
                speech_scores = {k: 0 for k in categories}

                # Parse Text into Tokens
                text_tokens = [
                    token for token in wordpunct_tokenize(text) if NON_PUNCT.match(token)
                ]

                # Skip analysis if text < 100 words
                if len(text_tokens) < 100:
                    continue

                # Word Count
                speech_scores['word_count'] = len(text_tokens)

                # Speaker Scores
                category_counts = parser.parse(text_tokens)
                for k, v in category_counts.items():
                    speech_scores[k+'_wc'] = v
                    speech_scores[k] = round(v/len(text_tokens), 5)

                # Build Score Dictionary
                speech_scores['speech_id'] = row_columns[0]
                speech_scores['speaker_id'] = row_columns[1]
                speech_scores['unit'] = row_columns[3]
                speech_scores['date'] = row_columns[2]
                writer.writerow(speech_scores)

        outfiles.append(
            OUTPUT_FILE.format(
                corpus=corpus,
                dictionary=dictionary,
                file=file.split('.')[0]
            )
        )
        LOGGER.debug("File Committed")
        LOGGER.debug(outfiles)

    return outfiles
