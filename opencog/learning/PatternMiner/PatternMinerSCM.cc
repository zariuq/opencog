/*
 * PatternMinerSCM.cc
 *
 * Copyright (C) 2017 OpenCog Foundation
 *
 * Author: Shujing Ke
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */


// The header file bits.

#ifndef _OPENCOG_PATTERNMINER_SCM_H
#define _OPENCOG_PATTERNMINER_SCM_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/learning/PatternMiner/PatternMiner.h>

#include <boost/algorithm/string.hpp>

using namespace std;
using namespace opencog::PatternMining;

namespace opencog
{

class PatternMinerSCM
{
private:
    static void* init_in_guile(void*);
    static void init_in_module(void*);
    void init(void);

    PatternMiner* patternMiner;

public:
    PatternMinerSCM();

    void run_patternminer()
    {
        patternMiner->runPatternMiner(false);
    }

    string get_Pattern_Max_Gram()
    {
        return  "max_gram: " + std::to_string( (int)(patternMiner->get_Pattern_Max_Gram()));
    }
    string set_Pattern_Max_Gram(int _max_gram)
    {
        patternMiner->set_Pattern_Max_Gram(_max_gram);
        return get_Pattern_Max_Gram();
    }

    string get_Enable_Interesting_Pattern()
    {
        bool enable = patternMiner->get_Enable_Interesting_Pattern();

        if (enable)
            return "enable_Interesting_Pattern: true";
        else
            return "enable_Interesting_Pattern: false";
    }

    string set_Enable_Interesting_Pattern(bool _enable)
    {
        patternMiner->set_Enable_Interesting_Pattern(_enable) ;
        return get_Enable_Interesting_Pattern();
    }

    string get_Frequency_threshold() {return "Frequency_threshold: " + std::to_string(patternMiner->get_Frequency_threshold());}
    string set_Frequency_threshold(int _Frequency_threshold)
    {
        patternMiner->set_Frequency_threshold(_Frequency_threshold);
        return get_Frequency_threshold();
    }


    string get_use_keyword_black_list()
    {
        bool enable = patternMiner->get_use_keyword_black_list();

        if (enable)
            return "use_keyword_black_list: true";
        else
            return "use_keyword_black_list: false";
    }

    string set_use_keyword_black_list(bool _use)
    {
        patternMiner->set_use_keyword_black_list(_use);
        return get_use_keyword_black_list();
    }

    string get_use_keyword_white_list()
    {
        bool enable = patternMiner->get_use_keyword_white_list();

        if (enable)
            return "use_keyword_white_list: true";
        else
            return "use_keyword_white_list: false";
    }

    string set_use_keyword_white_list(bool _use)
    {
        patternMiner->set_use_keyword_white_list(_use);
        return get_use_keyword_white_list();
    }

    string get_Ignore_Link_Types()
    {
        string result =  "Ignore_Link_Types:";
        vector<Type> ignore_list = patternMiner->get_Ignore_Link_Types();
        for (Type type : ignore_list)
            result +=  " " + classserver().getTypeName(type);

        return result;
    }

    string add_Ignore_Link_Type(const string& _typeStr)
    {
        Type atomType = classserver().getType(_typeStr);
        if (atomType == NOTYPE)
            return "Error: Input type doesn't exist!";

        string result = "";

        if (patternMiner->add_Ignore_Link_Type(atomType))
            result += "Added!\n";
        else
            result += "Input type already exists in the ingnore Link list!\n";

        return result + get_Ignore_Link_Types();

    }

    string remove_Ignore_Link_Type(const string& _typeStr)
    {
        Type atomType = classserver().getType(_typeStr);
        if (atomType == NOTYPE)
            return "Error: Input type doesn't exist!";

        string result = "";
        if (patternMiner->remove_Ignore_Link_Type(atomType))
            result += "Removed!\n";
        else
            result += "Input type does not exist in the ingnore Link list!\n";

        return result + get_Ignore_Link_Types();

    }

    string get_keyword_black_list()
    {
        string result =  "keyword_black_list:";
        vector<string> black_list = patternMiner->get_keyword_black_list();
        for (string word : black_list)
            result +=  " " + word;

        return result;
    }

    string add_keyword_to_black_list(const string& _keyword)
    {
        string result = "";
        if (patternMiner->add_keyword_to_black_list(_keyword))
            result += "Added!\n";
        else
            result +=  "Input keyword already exists in the black list!\n";
        return result + get_keyword_black_list();
    }

    string add_keywords_to_black_list(const string& _keywordlist)
    {
        vector<string> keyword_list;
        string keywordstr = _keywordlist;
        keywordstr .erase(std::remove(keywordstr .begin(), keywordstr .end(), ' '), keywordstr .end());
        boost::split(keyword_list, keywordstr , boost::is_any_of(","));

        for (string keyword : keyword_list)
            add_keyword_to_black_list(keyword);

        return "Added!" + get_keyword_black_list();
    }

    string remove_keyword_from_black_list(const string& _keyword)
    {
        string result = "";
        if (patternMiner->remove_keyword_from_black_list(_keyword))
            result += "Removed!\n";
        else
            result +=  "Input keyword does not exist in the black list!\n";
        return result + get_keyword_black_list();
    }


    string get_keyword_white_list()
    {
        string result =  "keyword_white_list:";
        vector<string> white_list = patternMiner->get_keyword_white_list();
        for (string word : white_list)
            result +=  " " + word;

        return result;
    }


    string add_keyword_to_white_list(const string& _keyword)
    {
        string result = "";
        if (patternMiner->add_keyword_to_white_list(_keyword))
            result += "Added!\n";
        else
            result +=  "Input keyword already exists in the white list!\n";
        return result + get_keyword_white_list();
    }

    string add_keywords_to_white_list(const string& _keywordlist)
    {
        vector<string> keyword_list;
        string keywordstr = _keywordlist;
        keywordstr .erase(std::remove(keywordstr .begin(), keywordstr .end(), ' '), keywordstr .end());
        boost::split(keyword_list, keywordstr , boost::is_any_of(","));

        for (string keyword : keyword_list)
            add_keyword_to_white_list(keyword);

        return "Added!" + get_keyword_white_list();
    }

    string remove_keyword_from_white_list(const string& _keyword)
    {
        string result = "";
        if (patternMiner->remove_keyword_from_white_list(_keyword))
            result += "Removed!\n";
        else
            result +=  "Input keyword does not exist in the white list!\n";
        return result + get_keyword_white_list();
    }

    string get_keyword_white_list_logic()
    {
        string result = "keyword_white_list_logic:";

        QUERY_LOGIC logic = patternMiner->get_keyword_white_list_logic();
        if (logic == QUERY_LOGIC::AND)
            result += "AND";
        else if (logic == QUERY_LOGIC::OR)
            result += "OR";
        else
            result += "EXCEPTION";

        return result;
    }

    string set_keyword_white_list_logic(const string& logic)
    {

        if ((logic == "AND") || (logic == "And") || (logic == "and"))
        {
            patternMiner->set_keyword_white_list_logic(QUERY_LOGIC::AND);
            return get_keyword_white_list_logic();
        }
        else if ((logic == "OR") || (logic == "Or") || (logic == "or"))
        {
            patternMiner->set_keyword_white_list_logic(QUERY_LOGIC::OR);
            return get_keyword_white_list_logic();
        }
        else
            return "Exception: keyword_white_list_logic only can be AND or OR.";
    }


    void clear_keyword_black_list(){patternMiner->clear_keyword_white_list();}
    void clear_keyword_white_list(){patternMiner->clear_keyword_white_list();}

    string set_enable_filter_node_types_should_not_be_vars(bool _enable)
    {
        patternMiner->set_enable_filter_node_types_should_not_be_vars(_enable);
        return get_enable_filter_node_types_should_not_be_vars();
    }

    string get_enable_filter_node_types_should_not_be_vars()
    {
        bool enable = patternMiner->get_enable_filter_node_types_should_not_be_vars();

        if (enable)
            return "enable_filter_node_types_should_not_be_vars: true";
        else
            return "enable_filter_node_types_should_not_be_vars: false";
    }

    string get_node_types_should_not_be_vars()
    {
        string result =  "node_types_should_not_be_vars:";
        vector<Type> _list = patternMiner->get_node_types_should_not_be_vars();
        for (Type type : _list)
            result +=  " " + classserver().getTypeName(type);

        return result;
    }

    string add_node_type_to_node_types_should_not_be_vars(const string& _typeStr)
    {
        Type atomType = classserver().getType(_typeStr);
        if (atomType == NOTYPE)
            return "Error: Input type doesn't exist!";

        string result = "";

        if (patternMiner->add_node_type_to_node_types_should_not_be_vars(atomType))
            result += "Added!\n";
        else
            result += "Input type already exists in the node_types_should_not_be_vars list!\n";

        return result + get_node_types_should_not_be_vars();

    }

    string remove_node_type_from_node_types_should_not_be_vars(const string& _typeStr)
    {
        Type atomType = classserver().getType(_typeStr);
        if (atomType == NOTYPE)
            return "Error: Input type doesn't exist!";

        string result = "";
        if (patternMiner->remove_node_type_from_node_types_should_not_be_vars(atomType))
            result += "Removed!\n";
        else
            result += "Input type does not exist in the node_types_should_not_be_vars list!\n";

        return result + get_node_types_should_not_be_vars();

    }

    void clear_node_types_should_not_be_vars(){patternMiner->clear_node_types_should_not_be_vars();}




    string set_enable_filter_links_of_same_type_not_share_second_outgoing(bool _enable)
    {
        patternMiner->set_enable_filter_links_of_same_type_not_share_second_outgoing(_enable);
        return get_enable_filter_links_of_same_type_not_share_second_outgoing();
    }

    string get_enable_filter_links_of_same_type_not_share_second_outgoing()
    {
        bool enable = patternMiner->get_enable_filter_links_of_same_type_not_share_second_outgoing();

        if (enable)
            return "enable_filter_links_of_same_type_not_share_second_outgoing: true";
        else
            return "enable_filter_links_of_same_type_not_share_second_outgoing: false";
    }

    string get_same_link_types_not_share_second_outgoing()
    {
        string result =  "same_link_types_not_share_second_outgoing Link_Types:";
        vector<Type> _list = patternMiner->get_same_link_types_not_share_second_outgoing();
        for (Type type : _list)
            result +=  " " + classserver().getTypeName(type);

        return result;
    }

    string add_link_type_to_same_link_types_not_share_second_outgoing(const string& _typeStr)
    {
        Type atomType = classserver().getType(_typeStr);
        if (atomType == NOTYPE)
            return "Error: Input type doesn't exist!";

        string result = "";

        if (patternMiner->add_link_type_to_same_link_types_not_share_second_outgoing(atomType))
            result += "Added!\n";
        else
            result += "Input type already exists in the same_link_types_not_share_second_outgoing list!\n";

        return result + get_same_link_types_not_share_second_outgoing();

    }

    string remove_link_type_from_same_link_types_not_share_second_outgoing(const string& _typeStr)
    {
        Type atomType = classserver().getType(_typeStr);
        if (atomType == NOTYPE)
            return "Error: Input type doesn't exist!";

        string result = "";
        if (patternMiner->remove_link_type_from_same_link_types_not_share_second_outgoing(atomType))
            result += "Removed!\n";
        else
            result += "Input type does not exist in the same_link_types_not_share_second_outgoing list!\n";

        return result + get_same_link_types_not_share_second_outgoing();

    }

    void clear_same_link_types_not_share_second_outgoing(){patternMiner->clear_same_link_types_not_share_second_outgoing();}


    string get_current_settings()
    {
        string result = "Current all settings:\n";
        result += get_Pattern_Max_Gram() + "\n";
        result += get_Enable_Interesting_Pattern() + "\n";
        result += get_Frequency_threshold() + "\n";
        result += get_Ignore_Link_Types() + "\n";
        result += get_use_keyword_black_list() + "\n";
        result += get_use_keyword_white_list() + "\n";
        result += get_keyword_black_list() + "\n";
        result += get_keyword_white_list() + "\n";
        result += get_keyword_white_list_logic() + "\n";
        result += get_enable_filter_links_of_same_type_not_share_second_outgoing() + "\n";
        result += get_same_link_types_not_share_second_outgoing()  + "\n";
        result += get_enable_filter_node_types_should_not_be_vars()  + "\n";
        result += get_node_types_should_not_be_vars() + "\n";

        return result;

    }

    // select a subset from the current AtomSpace with a list of keywords,splitted by ','.
    // the subset will contain max_distance connected atoms of these keywords
    // e.g.: "dog,cat,bike,swimming pool,computer"
    void select_subset_from_atomspace(const string& _keywordlist, int max_distance, bool if_contain_logic)
    {
        vector<string> keyword_list;
        string keywordstr = _keywordlist;
        keywordstr .erase(std::remove(keywordstr .begin(), keywordstr .end(), ' '), keywordstr .end());
        boost::split(keyword_list, keywordstr , boost::is_any_of(","));

        if (keyword_list.size() == 0)
        {
            cout << "\nError: Keyword list is empty!" << std::endl;
            return;
        }

        if (max_distance <= 0)
        {
            max_distance = patternMiner->get_Pattern_Max_Gram();
            cout <<"\n Becasue max_distance <= 0, Pattern_Max_Gram = " << max_distance << " will be used!" << std::endl;
        }

        patternMiner->selectSubsetFromCorpus(keyword_list, max_distance, if_contain_logic);
    }

    // select a subset from the current AtomSpace with a list of keywords,splitted by ','.
    // the subset will contain max_distance connected atoms which contains these keywords in their labels
    // e.g.: "book,computer", Nodes with labels : "book", "handbook", "computer", "super computer" will all be selected.
    void select_subset_from_atomspace_nodes_contain_keywords(const string& _keywordlist, int max_distance)
    {
        select_subset_from_atomspace(_keywordlist, max_distance, true);
    }


    // select a subset from the current AtomSpace with a list of keywords,splitted by ','.
    // the subset will contain max_distance connected atoms which contains these keywords in their labels
    // e.g.: "book,computer", only Nodes with labels : "book", "computer" will all be selected;
    // "handbook", "super computer"  will not be selected
    void select_subset_from_atomspace_nodes_equalto_keywords(const string& _keywordlist, int max_distance)
    {
        select_subset_from_atomspace(_keywordlist, max_distance, false);
    }

    void select_whitelist_subset_from_atomspace(int max_distance, bool if_contain_logic)
    {
        vector<string> keyword_list = patternMiner->get_keyword_white_list();
        if (keyword_list.size() == 0)
        {
            cout << "\nError: white keyword list is empty!" << std::endl;
            return;
        }

        if (max_distance <= 0)
        {
            max_distance = patternMiner->get_Pattern_Max_Gram();
            cout <<"\n Becasue max_distance <= 0, Pattern_Max_Gram = " << max_distance << " will be used!" << std::endl;
        }

        patternMiner->selectSubsetFromCorpus(keyword_list, max_distance, if_contain_logic);
    }

    // select a subset from the current AtomSpace with the keywords defined in whitelist.
    // the subset will contain max_distance connected atoms which contains these keywords in their labels
    // e.g.: "book,computer", Nodes with labels : "book", "handbook", "computer", "super computer" will all be selected.
    void select_whitelist_subset_from_atomspace_contain_keywords(int max_distance)
    {
        select_whitelist_subset_from_atomspace(max_distance, true);
    }

    // select a subset from the current AtomSpace with the keywords defined in whitelist.
    // the subset will contain max_distance connected atoms which contains these keywords in their labels
    // e.g.: "book,computer", only Nodes with labels : "book", "computer" will all be selected;
    // "handbook", "super computer"  will not be selected
    void select_whitelist_subset_from_atomspace_equalto_keywords(int max_distance)
    {
        select_whitelist_subset_from_atomspace(max_distance, false);
    }


    void apply_whitelist_keyword_filter_after_mining()
    {
        patternMiner->applyWhiteListKeywordfilterAfterMining();
    }


    // Note: this will release all the previous pattern mining results
    void reset_patternminer(bool resetAllSettingsFromConfig)
    {
        patternMiner->resetPatternMiner(resetAllSettingsFromConfig);
    }

    void run_interestingness_evaluation()
    {
        patternMiner->runInterestingnessEvaluation();
    }

    void load_patterns_from_result_file(const string& fileName)
    {
        patternMiner->loadPatternsFromResultFile(fileName);
    }
};


extern "C" {
void opencog_patternminer_init(void);
};

}
#endif // _OPENCOG_PATTERNMINER_SCM_H

// --------------------------------------------------------------

#include <opencog/util/Config.h>
#include <opencog/guile/SchemePrimitive.h>

/**
 * Implement a dynamically-loadable Pattern Miner guile module.
 */

using namespace opencog;


PatternMinerSCM::PatternMinerSCM()
{
    static bool is_init = false;
    if (is_init) return;
    is_init = true;
    scm_with_guile(init_in_guile, this);
}

void* PatternMinerSCM::init_in_guile(void* self)
{
    scm_c_define_module("opencog patternminer", init_in_module, self);
    scm_c_use_module("opencog patternminer");
    return NULL;
}

void PatternMinerSCM::init_in_module(void* data)
{
    PatternMinerSCM* self = (PatternMinerSCM*) data;
    self->init();
}

/**
 * The main init function for the PatternMinerSCM object.
 */
void PatternMinerSCM::init()
{
    AtomSpace* as = SchemeSmob::ss_get_env_as("patten miner");
    patternMiner = new PatternMiner(as);

    //---------------Note-----------------
    //
    // If you get compile error here, please pull the AtomSpace and make intall it.
    // git pull https://github.com/opencog/atomspace.git master
    //------------------------------------
    define_scheme_primitive("pm-run-patternminer", &PatternMinerSCM::run_patternminer, this, "patternminer");

    define_scheme_primitive("pm-get-current-settings", &PatternMinerSCM::get_current_settings, this, "patternminer");
    define_scheme_primitive("pm-get-pattern-max-gram", &PatternMinerSCM::get_Pattern_Max_Gram, this, "patternminer");
    define_scheme_primitive("pm-set-pattern-max-gram", &PatternMinerSCM::set_Pattern_Max_Gram, this, "patternminer");
    define_scheme_primitive("pm-get-enable-interesting-pattern", &PatternMinerSCM::get_Enable_Interesting_Pattern, this, "patternminer");
    define_scheme_primitive("pm-set-enable-interesting-pattern", &PatternMinerSCM::set_Enable_Interesting_Pattern, this, "patternminer");
    define_scheme_primitive("pm-get-frequency-threshold", &PatternMinerSCM::get_Frequency_threshold, this, "patternminer");
    define_scheme_primitive("pm-set-frequency-threshold", &PatternMinerSCM::set_Frequency_threshold, this, "patternminer");
    define_scheme_primitive("pm-get-ignore-link-types", &PatternMinerSCM::get_Ignore_Link_Types, this, "patternminer");
    define_scheme_primitive("pm-add-ignore-link-type", &PatternMinerSCM::add_Ignore_Link_Type, this, "patternminer");


    define_scheme_primitive("pm-get-enable-filter-node-types-should-not-be-vars",
                            &PatternMinerSCM::get_enable_filter_node_types_should_not_be_vars, this, "patternminer");
    define_scheme_primitive("pm-set-enable-filter-node-types-should-not-be-vars",
                            &PatternMinerSCM::set_enable_filter_node_types_should_not_be_vars, this, "patternminer");
    define_scheme_primitive("pm-get-node-types-should-not-be-vars",
                            &PatternMinerSCM::get_node_types_should_not_be_vars, this, "patternminer");
    define_scheme_primitive("pm-add-node-type-to-node-types-should-not-be-vars",
                            &PatternMinerSCM::add_node_type_to_node_types_should_not_be_vars, this, "patternminer");
    define_scheme_primitive("pm-remove-node-type-from-node-types-should-not-be-vars",
                            &PatternMinerSCM::remove_node_type_from_node_types_should_not_be_vars, this, "patternminer");
    define_scheme_primitive("pm-clear-node-types-should-not-be-vars",
                            &PatternMinerSCM::clear_node_types_should_not_be_vars, this, "patternminer");


    define_scheme_primitive("pm-get-enable-filter-links-of-same-type-not-share-second-outgoing",
                            &PatternMinerSCM::get_enable_filter_links_of_same_type_not_share_second_outgoing, this, "patternminer");
    define_scheme_primitive("pm-set-enable-filter-links-of-same-type-not-share-second-outgoing",
                            &PatternMinerSCM::set_enable_filter_links_of_same_type_not_share_second_outgoing, this, "patternminer");
    define_scheme_primitive("pm-get-same-link-types-not-share-second-outgoing",
                            &PatternMinerSCM::get_same_link_types_not_share_second_outgoing, this, "patternminer");
    define_scheme_primitive("pm-add-link-type-to-same-link-types-not-share-second-outgoing",
                            &PatternMinerSCM::add_link_type_to_same_link_types_not_share_second_outgoing, this, "patternminer");
    define_scheme_primitive("pm-remove-link-type-from-same-link-types-not-share-second-outgoing",
                            &PatternMinerSCM::remove_link_type_from_same_link_types_not_share_second_outgoing, this, "patternminer");
    define_scheme_primitive("pm-clear-same-link-types-not-share-second-outgoing",
                            &PatternMinerSCM::clear_same_link_types_not_share_second_outgoing, this, "patternminer");

    define_scheme_primitive("pm-remove-ignore-link-type", &PatternMinerSCM::remove_Ignore_Link_Type, this, "patternminer");
    define_scheme_primitive("pm-get-use-keyword-black-list", &PatternMinerSCM::get_use_keyword_black_list, this, "patternminer");
    define_scheme_primitive("pm-set-use-keyword-black-list", &PatternMinerSCM::set_use_keyword_black_list, this, "patternminer");
    define_scheme_primitive("pm-get-use-keyword-white-list", &PatternMinerSCM::get_use_keyword_white_list, this, "patternminer");
    define_scheme_primitive("pm-set-use-keyword-white-list", &PatternMinerSCM::set_use_keyword_white_list, this, "patternminer");
    define_scheme_primitive("pm-get-keyword-black-list", &PatternMinerSCM::get_keyword_black_list, this, "patternminer");
    define_scheme_primitive("pm-get-keyword-white-list", &PatternMinerSCM::get_keyword_white_list, this, "patternminer");
    define_scheme_primitive("pm-get-keyword-white-list-logic", &PatternMinerSCM::get_keyword_white_list_logic, this, "patternminer");
    define_scheme_primitive("pm-set-keyword-white-list-logic", &PatternMinerSCM::set_keyword_white_list_logic, this, "patternminer");
    define_scheme_primitive("pm-add-keyword-to-black-list", &PatternMinerSCM::add_keyword_to_black_list, this, "patternminer");
    define_scheme_primitive("pm-add-keywords-to-black-list", &PatternMinerSCM::add_keywords_to_black_list, this, "patternminer");
    define_scheme_primitive("pm-remove-keyword-from-black-list", &PatternMinerSCM::remove_keyword_from_black_list, this, "patternminer");
    define_scheme_primitive("pm-add-keyword-to-white-list", &PatternMinerSCM::add_keyword_to_white_list, this, "patternminer");
    define_scheme_primitive("pm-add-keywords-to-white-list", &PatternMinerSCM::add_keywords_to_white_list, this, "patternminer");
    define_scheme_primitive("pm-remove-keyword-from-white-list", &PatternMinerSCM::remove_keyword_from_white_list, this, "patternminer");
    define_scheme_primitive("pm-clear-keyword-black-list", &PatternMinerSCM::clear_keyword_black_list, this, "patternminer");
    define_scheme_primitive("pm-clear-keyword-white-list", &PatternMinerSCM::clear_keyword_white_list, this, "patternminer");

    define_scheme_primitive("pm-select-subset-from-atomspace-nodes-contain-keywords", &PatternMinerSCM::select_subset_from_atomspace_nodes_contain_keywords, this, "patternminer");
    define_scheme_primitive("pm-select-subset-from-atomspace-nodes-equalto-keywords", &PatternMinerSCM::select_subset_from_atomspace_nodes_equalto_keywords, this, "patternminer");
    define_scheme_primitive("pm-select-whitelist-subset-from-atomspace-contain-keywords", &PatternMinerSCM::select_whitelist_subset_from_atomspace_contain_keywords, this, "patternminer");
    define_scheme_primitive("pm-select-whitelist-subset-from-atomspace-equalto-keywords", &PatternMinerSCM::select_whitelist_subset_from_atomspace_equalto_keywords, this, "patternminer");
    define_scheme_primitive("pm-apply-whitelist-keyword-filter-after-mining", &PatternMinerSCM::apply_whitelist_keyword_filter_after_mining, this, "patternminer");
    define_scheme_primitive("pm-reset-patternminer", &PatternMinerSCM::reset_patternminer, this, "patternminer");
    define_scheme_primitive("pm-run-interestingness-evaluation", &PatternMinerSCM::run_interestingness_evaluation, this, "patternminer");
    define_scheme_primitive("pm-load-patterns-from-result-file", &PatternMinerSCM::load_patterns_from_result_file, this, "patternminer");


}

extern "C" {
void opencog_patternminer_init(void)
{
    static PatternMinerSCM patternminerscm;
}
};

// --------------------------------------------------------------

