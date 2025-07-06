#!/usr/bin/env bash
# unified-test-runner.sh - Unified test runner for TRAMP Supernote

# Change to the supernote-tramp directory
cd "$(dirname "$0")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test configuration
declare -A TEST_CATEGORIES
declare -A TEST_CONFIGS

# Core tests (should always pass, no network required)
TEST_CATEGORIES[core]="syntax mock json dot-entries tramp-core tramp-parsing error-handling cache-management"
TEST_CONFIGS[syntax]="syntax_check|Syntax Validation|emacs --batch --eval '(check-parens)' supernote-tramp.el"
TEST_CONFIGS[mock]="ert_test|Mock Server Tests|emacs --batch -l supernote-tramp.el -l tests/mock/test-supernote-tramp-mock.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[json]="ert_test|JSON Conversion Tests|emacs --batch -l tests/test-json-conversion.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[dot-entries]="ert_test|Directory Navigation (Dot Entries)|emacs --batch -l tests/test-dot-entries.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[tramp-core]="ert_test|TRAMP Core Tests|emacs --batch -l supernote-tramp.el -l tests/supernote-tramp-test-config.el -l tests/tramp/test-supernote-tramp.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[tramp-parsing]="ert_test|TRAMP Parsing Tests|cd tests/tramp && emacs --batch -l ../../supernote-tramp.el -l test-tramp-parsing.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[error-handling]="ert_test|Error Handling Tests|emacs --batch -l supernote-tramp.el -l tests/test-error-handling-ert.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[cache-management]="ert_test|Cache Management Tests|emacs --batch -l supernote-tramp.el -l tests/test-cache-management-ert.el -f ert-run-tests-batch-and-exit"

# Integration tests (require live server, all ERT tests)
TEST_CATEGORIES[integration]="readonly-ert dired-ert comprehensive-readonly-ert caching-performance-ert integration-basic"
TEST_CONFIGS[readonly-ert]="ert_test|Read-Only Operations|cd tests/tramp && timeout 60 emacs --batch -l ../../supernote-tramp.el -l test-readonly-and-copy-ert.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[dired-ert]="ert_test|Dired Integration|cd tests/dired && timeout 60 emacs --batch -l ../../supernote-tramp.el -l test-dired-functionality-ert.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[comprehensive-readonly-ert]="ert_test|Comprehensive Read-Only Tests|cd tests/tramp && timeout 60 emacs --batch -l ../../supernote-tramp.el -l test-comprehensive-readonly-ert.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[caching-performance-ert]="ert_test|Caching Performance Tests|cd tests/tramp && timeout 60 emacs --batch -l ../../supernote-tramp.el -l test-caching-performance-ert.el -f ert-run-tests-batch-and-exit"
TEST_CONFIGS[integration-basic]="ert_test|Integration Tests|cd tests/integration && timeout 30 emacs --batch -l ../../supernote-tramp.el -l test-supernote-tramp-integration.el -f ert-run-tests-batch-and-exit"

# Function to run a test
run_test() {
    local test_name="$1"
    local test_type="$2"
    local test_description="$3"
    local test_command="$4"
    
    echo -e "${YELLOW}Running $test_description...${NC}"
    
    # Run command based on type
    case "$test_type" in
        "syntax_check")
            if eval "$test_command" >/dev/null 2>&1; then
                echo -e "${GREEN}✓ $test_description PASSED${NC}"
                return 0
            else
                echo -e "${RED}✗ $test_description FAILED${NC}"
                return 1
            fi
            ;;
        "ert_test")
            if eval "$test_command" 2>&1 | grep -q "0 unexpected"; then
                echo -e "${GREEN}✓ $test_description PASSED${NC}"
                return 0
            else
                echo -e "${RED}✗ $test_description FAILED${NC}"
                return 1
            fi
            ;;
        *)
            echo -e "${RED}✗ Unknown test type: $test_type${NC}"
            return 1
            ;;
    esac
}

# Function to run a category of tests
run_category() {
    local category="$1"
    local tests="${TEST_CATEGORIES[$category]}"
    
    if [ -z "$tests" ]; then
        echo -e "${RED}Unknown test category: $category${NC}"
        return 1
    fi
    
    echo -e "${BLUE}=== Running $category tests ===${NC}"
    
    local passed=0
    local failed=0
    local total=0
    
    for test in $tests; do
        local config="${TEST_CONFIGS[$test]}"
        if [ -n "$config" ]; then
            IFS='|' read -r test_type test_description test_command <<< "$config"
            
            total=$((total + 1))
            if run_test "$test" "$test_type" "$test_description" "$test_command"; then
                passed=$((passed + 1))
            else
                failed=$((failed + 1))
            fi
            echo
        fi
    done
    
    echo -e "${BLUE}=== $category Results ===${NC}"
    printf "Total: %d, Passed: \033[0;32m%d\033[0m, Failed: \033[0;31m%d\033[0m\n" "$total" "$passed" "$failed"
    echo
    
    return $failed
}

# Main execution
main() {
    local category="${1:-core}"
    local show_help=false
    
    case "$category" in
        "--help"|"-h")
            show_help=true
            ;;
        "all")
            echo -e "${BLUE}=== TRAMP Supernote Complete Test Suite ===${NC}"
            echo "Running all test categories"
            echo
            
            local total_failed=0
            for cat in core integration; do
                run_category "$cat"
                total_failed=$((total_failed + $?))
            done
            
            echo -e "${BLUE}=== Overall Results ===${NC}"
            if [ $total_failed -eq 0 ]; then
                echo -e "${GREEN}🎉 All tests in all categories passed!${NC}"
                exit 0
            else
                echo -e "${RED}❌ Some tests failed ($total_failed categories with failures).${NC}"
                exit 1
            fi
            ;;
        "core"|"integration")
            echo -e "${BLUE}=== TRAMP Supernote Test Suite ===${NC}"
            echo "Running $category tests"
            echo
            
            run_category "$category"
            local exit_code=$?
            
            if [ $exit_code -eq 0 ]; then
                echo -e "${GREEN}🎉 All $category tests passed!${NC}"
            else
                echo -e "${RED}❌ Some $category tests failed.${NC}"
            fi
            exit $exit_code
            ;;
        *)
            show_help=true
            ;;
    esac
    
    if [ "$show_help" = true ]; then
        echo "Usage: $0 [category]"
        echo
        echo "Test categories:"
        echo "  core         - Core functionality tests (no network required)"
        echo "  integration  - Integration tests (require live server)"
        echo "  all          - Run all test categories"
        echo
        echo "Examples:"
        echo "  $0 core      - Run only core tests"
        echo "  $0 all       - Run all tests"
        echo "  $0           - Run core tests (default)"
        exit 0
    fi
}

main "$@"
